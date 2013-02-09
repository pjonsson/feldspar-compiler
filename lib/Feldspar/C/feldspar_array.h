//
// Copyright (c) 2009-2011, ERICSSON AB
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
//     * Redistributions of source code must retain the above copyright notice, 
//       this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in the
//       documentation and/or other materials provided with the distribution.
//     * Neither the name of the ERICSSON AB nor the names of its contributors
//       may be used to endorse or promote products derived from this software
//       without specific prior written permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

#ifndef FELDSPAR_ARRAY_H
#define FELDSPAR_ARRAY_H

#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>
//#define LOG
#include "log.h"

#include <stdio.h> /* to be removed */

/* Header size in bytes. */
#define HEADER_SIZE 16

/*
 * Header field offsets in number of 32 bit elements.
 */

/* Field for number of elements. */
#define LENGTH (-1)
/* Field for size in bytes for each element; -size for pointers. */
#define ELEM_SIZE (-2)
/* Field for size in bytes, including header. */
#define NUM_BYTES (-3)

/* Indexing into an array: */
#define at(arr,idx) (arr[idx])

/* Array (re)initialization */
static inline void *initArray(void *arr_in, int32_t size, int32_t len)
{
    int32_t newSize = abs(size);
    int32_t newBytes = len * newSize + HEADER_SIZE;
    int32_t *arr = arr_in ? (int32_t *) arr_in : malloc(newBytes) + HEADER_SIZE;

    log_3("initArray %p %d %d - enter\n", arr, size, len);
    assert(arr);
    arr[ELEM_SIZE] = size;
    arr[LENGTH] = len;
    if( arr_in )
    {
        // Re-initialization
        log_1("initArray %p - reinitialize\n", arr);
        if( arr[NUM_BYTES] < newBytes )
        {
            log_3("initArray %p - realloc since %d < %d\n"
		  , arr, arr[NUM_BYTES], newBytes);
            // Not enough space: reallocation needed
            arr[NUM_BYTES]  = newBytes;
            arr = realloc(arr - HEADER_SIZE, newBytes) + HEADER_SIZE;
        }
        else
        {
            // Otherwise: space is enough, nothing to do
            log_3("initArray %p - large enough %d >= %d\n"
                 , arr, arr[NUM_BYTES], newBytes);
        }
    }
    else
    {
        // First initialization
        arr[NUM_BYTES] = newBytes;
        log_4("initArray %p - alloc %d * %d = %d bytes\n"
	      , arr, arr[LENGTH], arr[ELEM_SIZE], newBytes);
    }
    assert( arr );
    log_3("initArray %p %d %d - leave\n", arr, size, len);
    return arr;
}

// Free array
// TODO: Think about arrays escaping from their scope.
static inline void freeArray(void *arr)
{
    log_1("freeArray %p - enter\n", arr);
    // assert(arr);
    // if( !arr->buffer )
    // {
        // return;
    // }
    // if( arr->elemSize < 0 )
    // {
        // int i;
        // for( i=0; i<arr->length; ++i )
            // freeArray( &at(struct array,arr,i) );
    // }
    // free(arr->buffer);
    // For the sake of extra safety:
    // arr->buffer = 0;
    // arr->length = 0;
    // arr->bytes = 0;
    log_1("freeArray %p - leave\n", arr);
}

/* Deep array copy */
static inline void copyArray(void *to_in, void *from_in)
{
    int32_t *to = (int32_t *)to_in;
    int32_t *from = (int32_t *)from_in;
    assert(to);
    assert(from);
    log_2("copyArray %p %p - enter\n", to, from);
    if( from[ELEM_SIZE] < 0 )
    {
        log_2("copyArray %p %p - nested enter\n", to, from);
        unsigned i;
        for( i = 0; i < from[LENGTH]; ++i )
        {
            void *to_row = &at(to, i);
            int32_t *from_row = &at(from, i);
            if( !to_row )
	      to_row = initArray( to_row, from_row[ELEM_SIZE], from_row[LENGTH] );
            copyArray( to_row, from_row );
        }
        log_2("copyArray %p %p - nested leave\n", to, from);
    }
    else
    {
        log_3("copyArray %p %p - memcpy %d bytes\n", to, from
	      , from[LENGTH] * from[ELEM_SIZE] );
        memcpy( to, from, from[LENGTH] * from[ELEM_SIZE] );
    }
    log_2("copyArray %p %p - leave\n", to, from);
}


/* Deep array copy with a given length */
void copyArrayLen(void *to, void *from, int32_t len);

/* Array length */
static inline int32_t getLength(void *arr)
{
  assert(arr);
  return ((int32_t *)arr)[LENGTH];
}

/* Set array length */
static inline void *setLength(void *arr_in, int32_t size, int32_t len)
{
    int32_t newSize = abs(size);
    int32_t newBytes = len * newSize + HEADER_SIZE;
    int32_t *arr = (int32_t *)(arr_in ? arr_in : initArray(arr_in, size, len));

    assert(arr);
    log_2("setLength %p %d - enter\n", arr, len);
    arr[ELEM_SIZE] = size;
    arr[LENGTH] = len;
    if( arr[NUM_BYTES] < newBytes )
    {
        log_3("setLength %p %d - realloc %d bytes\n", arr, len, newBytes);
        arr = realloc(arr - HEADER_SIZE, newBytes) + HEADER_SIZE;
        assert(arr);
        arr[NUM_BYTES] = newBytes;
    }
    log_2("setLength %p %d - leave\n", arr, len);
    return arr;
}

#endif
