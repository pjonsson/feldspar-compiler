--
-- Copyright (c) 2009-2011, ERICSSON AB
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--       this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the ERICSSON AB nor the names of its contributors
--       may be used to endorse or promote products derived from this software
--       without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Compiler.Imperative.FromCore.Interpretation where


import Control.Arrow
import Control.Monad.RWS
import Control.Applicative

import Data.Char (toLower)
import Data.List (intercalate, stripPrefix, nub)

import Feldspar.Range (fullRange)
import Feldspar.Core.UntypedRepresentation (VarId (..))

import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Imperative.Representation
import Feldspar.Compiler.Backend.C.Options (Options(..), Platform(..))

-- | Code generation monad
type CodeWriter = RWS CodeEnv CodeParts VarId

data CodeEnv = CodeEnv { aliases     :: [(VarId, Expression ())] -- ^ Variable aliasing
                       , inTask      :: Bool                     -- ^ Are we currently in a concurrent task?
                       , backendOpts :: Options                  -- ^ Options for the backend
                       }

initEnv :: Options -> CodeEnv
initEnv = CodeEnv [] False

data CodeParts = CodeParts { block    :: Block ()         -- ^ collects code within one block
                           , def      :: [Entity ()]      -- ^ collects top level definitions
                           , decl     :: [Declaration ()] -- ^ collects top level variable declarations
                           , params   :: [Variable ()]    -- ^ collects top level parameters
                           , epilogue :: [Program ()]     -- ^ collects postlude code (freeing memory, etc)
                           }

instance Monoid CodeParts
  where
    mempty      = CodeParts { block    = mempty
                            , def      = mempty
                            , decl     = mempty
                            , params   = mempty
                            , epilogue = mempty
                            }
    mappend a b = CodeParts { block    = mappend (block    a) (block    b)
                            , def      = mappend (def      a) (def      b)
                            , decl     = mappend (decl     a) (decl     b)
                            , params   = mappend (params   a) (params   b)
                            , epilogue = mappend (epilogue a) (epilogue b)
                            }

--------------------------------------------------------------------------------
-- * Utility functions
--------------------------------------------------------------------------------

-- | Make a struct type from a list of field names and their types
mkStructType :: [(String, Type)] -> Type
mkStructType trs = StructType n trs
  where
    n = "s_" ++ intercalate "_" (show (length trs):map (encodeType . snd) trs)

-- | Construct a named variable. The 'VarId' is appended to the base name to
-- allow different variables to have the same base name. Use a negative 'VarId'
-- to just get the base name without the appendix.
mkNamedVar
    :: String   -- ^ Base name
    -> Type     -- ^ Variable type
    -> VarId    -- ^ Identifier (appended to the base name)
    -> Variable ()
mkNamedVar base t i = Variable t $ base ++ if i < 0 then "" else show i

-- | Construct a named variable. The 'VarId' is appended to the base name to
-- allow different variables to have the same base name. Use a negative 'VarId'
-- to just get the base name without the appendix.
mkNamedRef
    :: String   -- ^ Base name
    -> Type     -- ^ Target type
    -> VarId    -- ^ Identifier (appended to the base name)
    -> Variable ()
mkNamedRef base t i = mkNamedVar base (1 :# (Pointer t)) i

-- | Construct a variable.
mkVariable :: Type -> VarId -> Variable ()
mkVariable t i | i >= 0 = mkNamedVar "v" t i

-- | Construct a pointer variable.
mkPointer :: Type -> VarId -> Variable ()
mkPointer t i | i >= 0 = mkNamedRef "v" t i

-- | Construct a variable expression.
mkVar :: Type -> VarId -> Expression ()
mkVar t = varToExpr . mkVariable t

-- | Generate a fresh identifier
freshId :: CodeWriter VarId
freshId = do
  v <- get
  put (v+1)
  return v

-- | Generate and declare a fresh uninitialized variable that will not be freed
-- in the postlude
freshAlias :: Type -> CodeWriter (Expression ())
freshAlias t = do i <- freshId
                  let v = mkNamedVar "e" t i
                  declareAlias v
                  return $ varToExpr v

-- | Generate and declare a fresh variable initialized to the given expression.
-- The variable will not be freed in the postlude.
freshAliasInit :: Expression () -> CodeWriter (Expression ())
freshAliasInit e = do vexp <- freshAlias (typeof e)
                      tellProg [Assign vexp e]
                      return vexp

-- | Declare an uninitialized variable that will be freed in the postlude (if
-- applicable)
declare :: Variable () -> CodeWriter ()
declare v = tellDeclWith True [Declaration v Nothing]

-- | Declare an uninitialized variable that will not be freed in the postlude
declareAlias :: Variable () -> CodeWriter ()
declareAlias v = tellDeclWith False [Declaration v Nothing]

-- | Declare and initialize a variable that will be freed in the postlude (if
-- applicable)
initialize :: Variable () -> Expression () -> CodeWriter ()
initialize v e = tellDeclWith True [Declaration v (Just e)]

-- | Add a definition to the generated program
tellDef :: [Entity ()] -> CodeWriter ()
tellDef es = tell $ mempty {def = es}

-- | Add a list of sub-programs to the generated program
tellProg :: [Program ()] -> CodeWriter ()
tellProg [BlockProgram b@(Block [] _)] = tell $ mempty {block = b}
tellProg ps = tell $ mempty {block = toBlock $ Sequence ps}

-- | Add a list of declarations to the generated program
tellDeclWith
    :: Bool  -- ^ Should arrays and IVars in the declarations be freed in the epilogue?
    -> [Declaration ()]
    -> CodeWriter ()
tellDeclWith free ds = do
    rs <- ask
    let frees | free = freeArrays ds ++ freeIVars ds
              | otherwise = []
        opts = backendOpts rs
        defs = getTypeDefs ds
        code | varFloating $ platform opts = mempty {decl = ds, epilogue = frees, def = defs}
             | otherwise = mempty {block = Block ds Empty,
                                   epilogue = frees, def = defs}
    tell code

{-
Encoded format is:

<type tag>_<inner_type_tag(s)>

Where the type tag is some unique prefix except for the scalar
types. The tag for StructTypes include the number of elements in the
struct to simplify the job for decodeType.

-}

encodeType :: Type -> String
encodeType = go
  where
    go VoidType              = "void"
    -- Machine vectors do not change memory layout, so keep internal.
    go (_ :# t)              = goScalar t
    go (IVarType t)          = "i_" ++ go t
    go (NativeArray _ t)     = "narr_" ++ go t
    go (StructType n _)      = n
    go (ArrayType _ t)       = "arr_" ++ go t
    goScalar BoolType        = "bool"
    goScalar BitType         = "bit"
    goScalar FloatType       = "float"
    goScalar DoubleType      = "double"
    goScalar (NumType s w)   = map toLower (show s) ++ show w
    goScalar (ComplexType t) = "complex_" ++ go t
    goScalar (Pointer t)     = "ptr_" ++ go t

-- Almost the inverse of encodeType. Some type encodings are lossy so
-- they are impossible to recover.
decodeType :: String -> [Type]
decodeType = goL []
  where
    goL acc [] = reverse acc
    goL acc s  = goL (out:acc) rest'
       where (out, rest) = go s
             rest' = case rest of
                      '_':t -> t
                      _     -> rest

    go (stripPrefix "void"     -> Just t) = (VoidType, t)
    go (stripPrefix "bool"     -> Just t) = (1 :# BoolType, t)
    go (stripPrefix "bit"      -> Just t) = (1 :# BitType, t)
    go (stripPrefix "float"    -> Just t) = (1 :# FloatType, t)
    go (stripPrefix "double"   -> Just t) = (1 :# DoubleType, t)
    go (stripPrefix "unsigned" -> Just t) = (1 :# (NumType Unsigned w), t')
     where (w, t') = decodeSize t
    go (stripPrefix "signed"   -> Just t) = (1 :# (NumType Signed w), t')
     where (w, t') = decodeSize t
    go (stripPrefix "complex"  -> Just t) = (1 :# (ComplexType tn), t')
     where (tn, t') = go t
    go (stripPrefix "ptr_"     -> Just t) = (1 :# (Pointer tt), t')
     where (tt, t') = go t
    go (stripPrefix "i_"       -> Just t) = (IVarType tt, t')
     where (tt, t') = go t
    go (stripPrefix "narr_"    -> Just t) = (NativeArray Nothing tt, t')
     where (tt, t') = go t
    go h@('s':'_':t) = (StructType h' $ zipWith mkMember [1..] ts, t'')
       where mkMember n t = ("member" ++ show n, t)
             Just (n, t') = decodeLen t
             (ts, t'') = structGo n t' []
             structGo 0 s acc = (reverse acc, s)
             structGo n ('_':s) acc = structGo (n - 1) s' (ts:acc)
                      where (ts, s') = go s
             h' = take (length h - length t'') h
    go (stripPrefix "arr_"     -> Just t) = (ArrayType fullRange tt, t')
      where (tt, t') = go t
    go s = error ("decodeType: " ++ s)

    decodeSize (stripPrefix "S32" -> Just t) = (S32, t)
    decodeSize (stripPrefix "S8"  -> Just t) = (S8, t)
    decodeSize (stripPrefix "S16" -> Just t) = (S16, t)
    decodeSize (stripPrefix "S40" -> Just t) = (S40, t)
    decodeSize (stripPrefix "S64" -> Just t) = (S64, t)

    decodeLen e
      | [p@(_,_)] <- reads e :: [(Int,String)]
      = Just p
      | otherwise = Nothing

-- | Find declarations that require top-level type definitions, and return those
-- definitions
getTypeDefs :: [Declaration ()] -> [Entity ()]
getTypeDefs defs = nub $ concatMap mkDef comps
  where
    comps = filter isComposite' $ map (typeof . declVar) defs
    -- There are other composite types that are not flagged as such by this
    -- version of isComposite, so keep it private.
    isComposite' :: Type -> Bool
    isComposite' (StructType {})               = True
    isComposite' (_ :# (Pointer t))            = isComposite' t
    isComposite' e                             = isArray e
    mkDef (StructType n members)
      =  concatMap (mkDef . snd) members
      ++ [StructDef n (map (uncurry StructMember) members)]
    mkDef (ArrayType _ typ)               = mkDef typ
    mkDef (_ :# (Pointer typ))            = mkDef typ
    mkDef _                               = []

-- | Copy an 'Expression' to a 'Location'. See 'copyProg' for more details.
assign :: Location -> Expression () -> CodeWriter ()
assign dst src = tellProg [copyProg dst [src]]

-- | Shallow assignment from an 'Expression' to a 'Location' (or nothing, if the
-- destination and source are identical)
--
-- Shallow assignment means that it becomes a plain assignment operation in the
-- generated code; i.e. no deep copying of structures.
shallowAssign :: Location -> Expression () -> CodeWriter ()
shallowAssign (Just dst) src | dst /= src = tellProg [Assign dst src]
shallowAssign _          _                = return ()

shallowCopyWithRefSwap :: Expression () -> Expression () -> CodeWriter ()
shallowCopyWithRefSwap dst src
  | dst /= src
  = case filter (hasReference . snd) $ flattenStructs $ typeof dst of
      [] -> tellProg [Assign dst src]
      arrs -> do temps <- sequence [freshAliasInit $ accF dst | (accF, _) <- arrs]
                 tellProg [Assign dst src]
                 tellProg [Assign (accF src) tmp | (tmp, (accF, _)) <- zip temps arrs]
  | otherwise = return ()

shallowCopyReferences :: Expression () -> Expression () -> CodeWriter ()
shallowCopyReferences dst src
  = tellProg [Assign (accF dst) (accF src)
                 | (accF, t) <- flattenStructs $ typeof dst, hasReference t]

{-
This function implements double buffering of state for imnmutable
seqential loops (forLoop and whileLoop).  The intention is to generate
the minimal amount of copying (especially deep array copies) while
also avoiding frequent references to data that can not be allocated in
registers.

The state of the loop is implemented by two variables so that the
loop body reads from one and writes to the other. At the end of the
body, the contents of the second (write) variable is shallow copied to
the first (read) variable. In order to avoid inadvertent sharing of
data referenced by pointers in the variables (array buffers, for
instance), the pointers in the state are swapped rather than just
copied so that the end up in the variable written to. Finally the read
variable is shallow copied to the result location.

There are some simplifying cases:
    - When the target lvalue loc is a variable, and thus cheap to
      access, it is reused as the read state variable
    - When the type of the state is scalar, so that assignment is
      atomic, only one state variable is used and it is both read and
      written to in the loop body, eliminating the shallow copy in the
      loop body.
The strategy implemented a compromise between different design constraints:
    - Avoid deep copying of arrays (that is what the double buffering is for)
    - Avoid shallow copying if possible
    - Avoid memory leaks of arrays and ivars
-}

mkDoubleBufferState :: Expression () -> VarId -> CodeWriter (Expression (), Expression ())
mkDoubleBufferState loc stvar
   = do stvar1 <- if isVarExpr loc || containsNativeArray (typeof loc)
                     then return loc
                     else do vexp <- freshAlias (typeof loc)
                             shallowCopyReferences vexp loc
                             return vexp
        stvar2 <- if isComposite $ typeof loc
                     then do let vexp2 = mkVariable (typeof loc) stvar
                             declare vexp2
                             return $ varToExpr vexp2
                     else return stvar1
        return (stvar1, stvar2)


-- | Move the generated code block from the 'CodeWriter' effect to the result
-- value. Top-level declarations etc. remain in the 'CodeWriter' effect.
confiscateBlock :: CodeWriter a -> CodeWriter (a, Block ())
confiscateBlock m
    = liftM (second block)
    $ censor (\rec -> rec {block = mempty})
    $ listen m

-- | Move the generated code block, declarations and postlude code from the
-- 'CodeWriter' effect to the result value. The other fields of 'CodeParts' remain
-- in the 'CodeWriter' effect.
--
-- Note that the resulting declarations and postlude code most probably need to
-- be re-emitted in the 'CodeWriter'. Otherwise there is a risk that the
-- generated code will be incorrect.
confiscateBigBlock ::
    CodeWriter a -> CodeWriter (a, (Block (), [Declaration ()], [Program ()]))
confiscateBigBlock m
    = liftM (\(a,ws) -> (a, (block ws, decl ws, epilogue ws)))
    $ censor (\rec -> rec {block = mempty, decl = mempty, epilogue = mempty})
    $ listen m

-- | Add an alias to the environment of a local code generator
withAlias
    :: VarId          -- ^ Variable to alias
    -> Expression ()  -- ^ Expression to substitute for the variable
    -> CodeWriter a   -- ^ Local code generator
    -> CodeWriter a
withAlias v0 expr = local (\e -> e {aliases = (v0,expr) : aliases e})

