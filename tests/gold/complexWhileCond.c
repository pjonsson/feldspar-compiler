#include "complexWhileCond.h"


void complexWhileCond(int32_t v0, struct s_2_signedS32_signedS32 * out)
{
  struct s_2_signedS32_signedS32 e10 = { .member1 = 0, .member2 = 0 };
  struct s_2_signedS32_signedS32 v9 = { .member1 = 0, .member2 = 0 };
  int32_t v3;
  int32_t v6;
  bool v2;
  
  (e10).member1 = 0;
  (e10).member2 = v0;
  v3 = (e10).member1;
  v6 = ((e10).member2 - v3);
  v2 = ((v3 * v3) != (v6 * v6));
  while (v2)
  {
    (v9).member1 = ((e10).member1 + 1);
    (v9).member2 = (e10).member2;
    e10 = v9;
    v3 = (e10).member1;
    v6 = ((e10).member2 - v3);
    v2 = ((v3 * v3) != (v6 * v6));
  }
  *out = e10;
}
