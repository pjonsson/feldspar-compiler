#include "scanlPush.h"
#include "feldspar_c99.h"
#include "feldspar_array.h"
#include "feldspar_future.h"
#include "ivar.h"
#include "taskpool.h"
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <stdbool.h>
#include <complex.h>


void scanlPush(uint32_t * v0, uint32_t * v1, uint32_t * * * out)
{
  uint32_t v22;
  uint32_t * v24 = NULL;
  uint32_t v23;
  uint32_t * v7 = NULL;
  uint32_t v25;
  uint32_t * v15 = NULL;
  uint32_t v26;
  uint32_t * e0 = NULL;
  
  v22 = getLength(v1);
  v23 = getLength(v0);
  v24 = setLength(v24, sizeof(uint32_t), v23);
  for (uint32_t v4 = 0; v4 < v23; v4 += 1)
  {
    at(v24,v4) = at(v0,v4);
  }
  *out = setLength(*out, (0 - sizeof(uint32_t *)), v22);
  v7 = initArray(v7, sizeof(uint32_t), getLength(v24));
  copyArray(v7, v24);
  for (uint32_t v8 = 0; v8 < v22; v8 += 1)
  {
    v25 = getLength(v7);
    v7 = setLength(v7, sizeof(uint32_t), v25);
    for (uint32_t v11 = 0; v11 < v25; v11 += 1)
    {
    }
    v15 = initArray(v15, sizeof(uint32_t), getLength(v7));
    copyArray(v15, v7);
    v26 = getLength(v15);
    e0 = setLength(e0, sizeof(uint32_t), v26);
    for (uint32_t v17 = 0; v17 < v26; v17 += 1)
    {
      at(e0,v17) = at(v15,v17);
    }
    at(*out,v8) = initArray(at(*out,v8), sizeof(uint32_t), getLength(e0));
    copyArray(at(*out,v8), e0);
  }
  freeArray(v24);
  freeArray(v7);
  freeArray(v15);
  freeArray(e0);
}
