#ifndef TESTS_FUT1_H
#define TESTS_FUT1_H

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


void task_core2(struct ivar e0);

void task2(void * params);

void fut1(struct ivar v0, struct ivar * out);

#endif // TESTS_FUT1_H