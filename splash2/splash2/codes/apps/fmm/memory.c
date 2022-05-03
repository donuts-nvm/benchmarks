
/*************************************************************************/
/*                                                                       */
/*  Copyright (c) 1994 Stanford University                               */
/*                                                                       */
/*  All rights reserved.                                                 */
/*                                                                       */
/*  Permission is given to use, copy, and modify this software for any   */
/*  non-commercial purpose as long as this copyright notice is not       */
/*  removed.  All other uses, including redistribution in whole or in    */
/*  part, are forbidden without prior written permission.                */
/*                                                                       */
/*  This software is provided with absolutely no warranty and no         */
/*  support.                                                             */
/*                                                                       */
/*************************************************************************/

#include "defs.h"
#include "memory.h"


#include <parmacs.h>
int ParmacsThreadNum = 0;
pthread_t ParmacsThreads[PARMACS_MAX_THREADS];


g_mem *G_Memory;
local_memory Local[MAX_PROCS];

/*
 *  InitGlobalMemory ()
 *
 *  Args : none.
 *
 *  Returns : nothing.
 *
 *  Side Effects : Allocates all the global storage for G_Memory.
 *
 */
void
InitGlobalMemory ()
{
   int i;

   G_Memory = (g_mem *) valloc(sizeof(g_mem));;
   G_Memory->i_array = (int *) valloc(Number_Of_Processors * sizeof(int));;
   G_Memory->d_array = (double *) valloc(Number_Of_Processors
					 * sizeof(double));;
   if (G_Memory == NULL) {
      printf("Ran out of global memory in InitGlobalMemory\n");
      exit(-1);
   }
   G_Memory->count = 0;
   G_Memory->id = 0;
   {pthread_mutex_init(&(G_Memory->io_lock), NULL);};
   {pthread_mutex_init(&(G_Memory->mal_lock), NULL);};
   {pthread_mutex_init(&(G_Memory->single_lock), NULL);};
   {pthread_mutex_init(&(G_Memory->count_lock), NULL);};
   {
	unsigned long	i, Error;

	for (i = 0; i < MAX_LOCKS; i++) {
		Error = pthread_mutex_init(&G_Memory->lock_array[i], NULL);
		if (Error != 0) {
			printf("Error while initializing array of locks.\n");
			exit(-1);
		}
	}
};
   {
	unsigned long	Error;

	Error = pthread_mutex_init(&(G_Memory->synch).mutex, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		exit(-1);
	}

	Error = pthread_cond_init(&(G_Memory->synch).cv, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		pthread_mutex_destroy(&(G_Memory->synch).mutex);
		exit(-1);
	}

	(G_Memory->synch).counter = 0;
	(G_Memory->synch).cycle = 0;
};
   G_Memory->max_x = -MAX_REAL;
   G_Memory->min_x = MAX_REAL;
   G_Memory->max_y = -MAX_REAL;
   G_Memory->min_y = MAX_REAL;
}


