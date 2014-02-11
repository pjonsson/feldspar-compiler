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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Compiler.Imperative.FromCore.ParFor where

import Data.Typeable

import Language.Syntactic
import Language.Syntactic.Constructs.Monad
import Language.Syntactic.Constructs.Binding hiding (Variable)
import Language.Syntactic.Constructs.Binding.HigherOrder

import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Binding hiding (Variable)
import Feldspar.Core.Constructs.ParFor
import Feldspar.Core.Constructs.Literal

import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Imperative.Representation (Block(..), Program(..),
                                                    Entity(..), typeof,
                                                    Expression(..), fv)
import Feldspar.Compiler.Imperative.FromCore.Interpretation

instance ( Compile dom dom
         , Render dom
         , Project (CLambda Type) dom
         , Project (Variable :|| Type) dom
         , Project (Literal :|| Type) dom
         , Project Let dom
         , Project (ParForFeat :|| Type) dom
         , ConstrainedBy dom Typeable
         , AlphaEq dom dom (Decor Info dom) [(VarId, VarId)]
         )
      => Compile (ParForFeat :|| Type) dom
  where
    compileExprSym = compileProgFresh

    compileProgSym (C' PParRun) _ loc (len :* arr :* Nil) = do
      let sz  = infoSize $ getInfo len
      len' <- mkLength len (infoType $ getInfo len) sz
      tellProg [initArray loc len']
      compileProg loc arr

    compileProgSym (C' PParFor) info loc (len :* (lam1 :$ step) :* (lam2 :$ ixf) :* Nil)
        | Just (SubConstr2 (Lambda v1)) <- prjLambda lam1
        , Just (SubConstr2 (Lambda v2)) <- prjLambda lam2
        = do
            let ta = argType $ infoType $ getInfo lam2
            let sa = fst $ infoSize $ getInfo lam2
            let ix = mkVar (compileTypeRep ta sa) v2
            len' <- mkLength len (infoType $ getInfo len) sa
            step' <- withAlias v1 ix $ compileExpr step
            (_, ixf') <- confiscateBlock $ compileProg loc ixf
            tellProg [for True (lName ix) len' step' ixf']

    compileProgSym (C' PParRed) info (Just loc) (len :* st0 :* (lam1 :$ lt1) :* Nil)
      | Just (SubConstr2 (Lambda v1)) <- prjLambda lam1
      , (bs1, lam2 :$ ixf)             <- collectLetBinders lt1
      , Just (SubConstr2 (Lambda s)) <- prjLambda lam2
      = do
          let ta = argType $ infoType $ getInfo lam1
              sa = fst $ infoSize $ getInfo lam1
              ix = mkVar (compileTypeRep ta sa) v1
              tst = infoType $ getInfo ixf
              sst = infoSize $ getInfo ixf
          len' <- mkLength len (infoType $ getInfo len) sa
          st1 <- freshVar "st" tst sst
          let st = mkRef (compileTypeRep tst sst) s
          declareAlias st
          compileProg (Just st1) st0
          (_, Block bs ixf') <- confiscateBlock $ withAlias s st $ compileProg (Just $ ArrayElem loc ix) ixf
          tellProg [ Assign st st1
                   , for False (lName ix) len' (litI32 1) $
                       Block bs (Sequence [ixf', Assign st (ArrayElem loc ix)])]

    compileProgSym (C' PParPut) _ (Just loc) (ix :* e :* Nil) = do
      dst <- compileExpr ix
      compileProg (Just $ ArrayElem loc dst) e

    compileProgSym (C' PParComb) _ loc (p1 :* p2 :* Nil) = do
      ((_, ws1), Block ds1 b1) <- confiscateBigBlock $ compileProg loc p1
      ((_, ws2), Block ds2 b2) <- confiscateBigBlock $ compileProg loc p2

      tellProg [toProg $ Block (ds1 ++ ds2) (Sequence [b1,b2])]

    compileProgSym a info loc args = compileExprLoc a info loc args
