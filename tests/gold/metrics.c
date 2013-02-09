#include "metrics.h"
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


void metrics(int32_t * v0, int32_t * v1, struct s_unsignedS32_unsignedS32 * * v2, int32_t * * * out)
{
  uint32_t v20;
  uint32_t len0;
  int32_t * st1 = NULL;
  int32_t * * v17 = NULL;
  uint32_t len2;
  
  v20 = getLength(v0);
  len0 = getLength(v2);
  st1 = initArray(st1, sizeof(int32_t), 8);
  at(st1,0) = -32678;
  at(st1,1) = -32678;
  at(st1,2) = -32678;
  at(st1,3) = -32678;
  at(st1,4) = -32678;
  at(st1,5) = -32678;
  at(st1,6) = -32678;
  at(st1,7) = -32678;
  *v17 = st1;
  *out = initArray(*out, (0 - sizeof(int32_t *)), len0);
  for (uint32_t v16 = 0; v16 < len0; v16 += 1)
  {
    len2 = min(getLength(at(v2,v16)), v20);
    at(*out,v16) = initArray(at(*out,v16), sizeof(int32_t), len2);
    for (uint32_t v18 = 0; v18 < len2; v18 += 1)
    {
      at(at(*out,v16),v18) = at(*v17,(at(at(v2,v16),v18)).member1);
    }
    *v17 = at(*out,v16);
  }
  freeArray(st1);
  freeArray(*v17);
}
