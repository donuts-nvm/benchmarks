
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

/*************************************************************************/
/*                                                                       */
/*  SPLASH Ocean Code                                                    */
/*                                                                       */
/*  This application studies the role of eddy and boundary currents in   */
/*  influencing large-scale ocean movements.  This implementation uses   */
/*  dynamically allocated four-dimensional arrays for grid data storage. */
/*                                                                       */
/*  Command line options:                                                */
/*                                                                       */
/*     -nN : Simulate NxN ocean.  N must be (power of 2)+2.              */
/*     -pP : P = number of processors.  P must be power of 2.            */
/*     -eE : E = error tolerance for iterative relaxation.               */
/*     -rR : R = distance between grid points in meters.                 */
/*     -tT : T = timestep in seconds.                                    */
/*     -s  : Print timing statistics.                                    */
/*     -o  : Print out relaxation residual values.                       */
/*     -h  : Print out command line options.                             */
/*                                                                       */
/*  Default: OCEAN -n130 -p1 -e1e-7 -r20000.0 -t28800.0                  */
/*                                                                       */
/*  NOTE: This code works under both the FORK and SPROC models.          */
/*                                                                       */
/*************************************************************************/


#include <parmacs.h>
int ParmacsThreadNum = 0;
pthread_t ParmacsThreads[PARMACS_MAX_THREADS];


#define DEFAULT_N      258
#define DEFAULT_P        1
#define DEFAULT_E        1e-7
#define DEFAULT_T    28800.0
#define DEFAULT_R    20000.0
#define UP               0
#define DOWN             1
#define LEFT             2
#define RIGHT            3
#define UPLEFT           4
#define UPRIGHT          5
#define DOWNLEFT         6
#define DOWNRIGHT        7
#define PAGE_SIZE     4096

#include <stdio.h>
#include <math.h>
#include <time.h>

struct multi_struct {
   double err_multi;
} *multi;

struct global_struct {
   int id;
   int starttime;
   int trackstart;
   double psiai;
   double psibi;
} *global;

double ****psi;
double ****psim;
double ***psium;
double ***psilm;
double ***psib;
double ***ga;
double ***gb;
double ****work1;
double ***work2;
double ***work3;
double ****work4;
double ****work5;
double ***work6;
double ****work7;
double ****temparray;
double ***tauz;
double ***oldga;
double ***oldgb;
double *f;
double ****q_multi;
double ****rhs_multi;

struct locks_struct {
   pthread_mutex_t (idlock);
   pthread_mutex_t (psiailock);
   pthread_mutex_t (psibilock);
   pthread_mutex_t (donelock);
   pthread_mutex_t (error_lock);
   pthread_mutex_t (bar_lock);
} *locks;

struct bars_struct {
   
struct {
	pthread_mutex_t	mutex;
	pthread_cond_t	cv;
	unsigned long	counter;
	unsigned long	cycle;
} (iteration);

   
struct {
	pthread_mutex_t	mutex;
	pthread_cond_t	cv;
	unsigned long	counter;
	unsigned long	cycle;
} (gsudn);

   
struct {
	pthread_mutex_t	mutex;
	pthread_cond_t	cv;
	unsigned long	counter;
	unsigned long	cycle;
} (p_setup);
 
   
struct {
	pthread_mutex_t	mutex;
	pthread_cond_t	cv;
	unsigned long	counter;
	unsigned long	cycle;
} (p_redph);
 
   
struct {
	pthread_mutex_t	mutex;
	pthread_cond_t	cv;
	unsigned long	counter;
	unsigned long	cycle;
} (p_soln);
 
   
struct {
	pthread_mutex_t	mutex;
	pthread_cond_t	cv;
	unsigned long	counter;
	unsigned long	cycle;
} (p_subph);
 
   
struct {
	pthread_mutex_t	mutex;
	pthread_cond_t	cv;
	unsigned long	counter;
	unsigned long	cycle;
} (sl_prini);

   
struct {
	pthread_mutex_t	mutex;
	pthread_cond_t	cv;
	unsigned long	counter;
	unsigned long	cycle;
} (sl_psini);

   
struct {
	pthread_mutex_t	mutex;
	pthread_cond_t	cv;
	unsigned long	counter;
	unsigned long	cycle;
} (sl_onetime);

   
struct {
	pthread_mutex_t	mutex;
	pthread_cond_t	cv;
	unsigned long	counter;
	unsigned long	cycle;
} (sl_phase_1);

   
struct {
	pthread_mutex_t	mutex;
	pthread_cond_t	cv;
	unsigned long	counter;
	unsigned long	cycle;
} (sl_phase_2);

   
struct {
	pthread_mutex_t	mutex;
	pthread_cond_t	cv;
	unsigned long	counter;
	unsigned long	cycle;
} (sl_phase_3);

   
struct {
	pthread_mutex_t	mutex;
	pthread_cond_t	cv;
	unsigned long	counter;
	unsigned long	cycle;
} (sl_phase_4);

   
struct {
	pthread_mutex_t	mutex;
	pthread_cond_t	cv;
	unsigned long	counter;
	unsigned long	cycle;
} (sl_phase_5);

   
struct {
	pthread_mutex_t	mutex;
	pthread_cond_t	cv;
	unsigned long	counter;
	unsigned long	cycle;
} (sl_phase_6);

   
struct {
	pthread_mutex_t	mutex;
	pthread_cond_t	cv;
	unsigned long	counter;
	unsigned long	cycle;
} (sl_phase_7);

   
struct {
	pthread_mutex_t	mutex;
	pthread_cond_t	cv;
	unsigned long	counter;
	unsigned long	cycle;
} (sl_phase_8);

   
struct {
	pthread_mutex_t	mutex;
	pthread_cond_t	cv;
	unsigned long	counter;
	unsigned long	cycle;
} (sl_phase_9);

   
struct {
	pthread_mutex_t	mutex;
	pthread_cond_t	cv;
	unsigned long	counter;
	unsigned long	cycle;
} (sl_phase_10);

   
struct {
	pthread_mutex_t	mutex;
	pthread_cond_t	cv;
	unsigned long	counter;
	unsigned long	cycle;
} (error_barrier);

} *bars;

void subblock();
void slave();
int log_2(int);
void printerr(char *);

int nprocs = DEFAULT_P;
double h1 = 1000.0;
double h3 = 4000.0;
double h = 5000.0;
double lf = -5.12e11;
double res = DEFAULT_R;
double dtau = DEFAULT_T;
double f0 = 8.3e-5;
double beta = 2.0e-11;
double gpr = 0.02;
int im = DEFAULT_N;
int jm;
double tolerance = DEFAULT_E;
double eig2;
double ysca;
int jmm1;
double pi;
double t0 = 0.5e-4 ;
double outday0 = 1.0;
double outday1 = 2.0;
double outday2 = 2.0;
double outday3 = 2.0;
double factjacob;
double factlap;
int numlev;
int *imx;
int *jmx;
double *lev_res;
double *lev_tol;
double maxwork = 10000.0;

struct Global_Private {
  char pad[PAGE_SIZE];
  int *rel_num_x;
  int *rel_num_y;
  int *eist;     
  int *ejst;     
  int *oist;     
  int *ojst;     
  int *rlist;    
  int *rljst;    
  int *rlien;    
  int *rljen;    
  int rownum;
  int colnum;
  int neighbors[8];
  double multi_time;
  double total_time;
} *gp;

double *i_int_coeff;
double *j_int_coeff;
int xprocs;
int yprocs;
int *xpts_per_proc;
int *ypts_per_proc;
int minlevel;
int do_stats = 0;
int do_output = 0;

void main(argc, argv)

int argc;
char *argv[];

{
   int i;
   int j;
   int k;
   double work_multi;
   int my_num;
   int x_part;
   int y_part;
   int d_size;
   int itemp;
   int jtemp;
   double procsqrt;
   FILE *fileptr;
   int iindex;
   int temp = 0;
   char c;
   double min_total;
   double max_total;
   double avg_total;
   double min_multi;
   double max_multi;
   double avg_multi;
   double min_frac;
   double max_frac;
   double avg_frac;
   int ch;
   extern char *optarg;
   unsigned int computeend;
   unsigned int start;

   {
	struct timeval	FullTime;

	gettimeofday(&FullTime, NULL);
	(start) = (unsigned long)(FullTime.tv_usec + FullTime.tv_sec * 1000000);
}

   while ((ch = getopt(argc, argv, "n:p:e:r:t:soh")) != -1) {
     switch(ch) {
     case 'n': im = atoi(optarg);
               if (log_2(im-2) == -1) {
                 printerr("Grid must be ((power of 2)+2) in each dimension\n");
                 exit(-1);
               }
               break;
     case 'p': nprocs = atoi(optarg);
               if (nprocs < 1) {
                 printerr("P must be >= 1\n");
                 exit(-1);
               }
               if (log_2(nprocs) == -1) {
                 printerr("P must be a power of 2\n");
                 exit(-1);
               }
               break;
     case 'e': tolerance = atof(optarg); break;
     case 'r': res = atof(optarg); break;
     case 't': dtau = atof(optarg); break;
     case 's': do_stats = !do_stats; break;
     case 'o': do_output = !do_output; break;
     case 'h': printf("Usage: OCEAN <options>\n\n");
               printf("options:\n");
               printf("  -nN : Simulate NxN ocean.  N must be (power of 2)+2.\n");
               printf("  -pP : P = number of processors.  P must be power of 2.\n");
               printf("  -eE : E = error tolerance for iterative relaxation.\n");
               printf("  -rR : R = distance between grid points in meters.\n");
               printf("  -tT : T = timestep in seconds.\n");
               printf("  -s  : Print timing statistics.\n");
               printf("  -o  : Print out relaxation residual values.\n");
               printf("  -h  : Print out command line options.\n\n");
               printf("Default: OCEAN -n%1d -p%1d -e%1g -r%1g -t%1g\n",
                       DEFAULT_N,DEFAULT_P,DEFAULT_E,DEFAULT_R,DEFAULT_T);
               exit(0);
               break;
     }
   }

   {;} 

   jm = im;
   printf("\n");
   printf("Ocean simulation with W-cycle multigrid solver\n");
   printf("    Processors                         : %1d\n",nprocs);
   printf("    Grid size                          : %1d x %1d\n",im,jm);
   printf("    Grid resolution (meters)           : %0.2f\n",res);
   printf("    Time between relaxations (seconds) : %0.0f\n",dtau);
   printf("    Error tolerance                    : %0.7g\n",tolerance);
   printf("\n");

   xprocs = 0;
   yprocs = 0;
   procsqrt = sqrt((double) nprocs);
   j = (int) procsqrt;
   while ((xprocs == 0) && (j > 0)) {
     k = nprocs / j;
     if (k * j == nprocs) {
       if (k > j) {
         xprocs = j;
         yprocs = k;
       } else {
         xprocs = k;
         yprocs = j;
       }
     }
     j--;
   }
   if (xprocs == 0) {
     printerr("Could not find factors for subblocking\n");
     exit(-1);
   }  

   minlevel = 0;
   itemp = 1;
   jtemp = 1;
   numlev = 0;
   minlevel = 0;
   while (itemp < (im-2)) {
     itemp = itemp*2;
     jtemp = jtemp*2;
     if ((itemp/yprocs > 1) && (jtemp/xprocs > 1)) {
       numlev++;
     }
   }  
   
   if (numlev == 0) {
     printerr("Must have at least 2 grid points per processor in each dimension\n");
     exit(-1);
   }

   imx = (int *) valloc(numlev*sizeof(int));;
   jmx = (int *) valloc(numlev*sizeof(int));;
   lev_res = (double *) valloc(numlev*sizeof(double));;
   lev_tol = (double *) valloc(numlev*sizeof(double));;
   i_int_coeff = (double *) valloc(numlev*sizeof(double));;
   j_int_coeff = (double *) valloc(numlev*sizeof(double));;
   xpts_per_proc = (int *) valloc(numlev*sizeof(int));;
   ypts_per_proc = (int *) valloc(numlev*sizeof(int));;

   imx[numlev-1] = im;
   jmx[numlev-1] = jm;
   lev_res[numlev-1] = res;
   lev_tol[numlev-1] = tolerance;

   for (i=numlev-2;i>=0;i--) {
     imx[i] = ((imx[i+1] - 2) / 2) + 2;
     jmx[i] = ((jmx[i+1] - 2) / 2) + 2;
     lev_res[i] = lev_res[i+1] * 2;
   }

   for (i=0;i<numlev;i++) {
     xpts_per_proc[i] = (jmx[i]-2) / xprocs;
     ypts_per_proc[i] = (imx[i]-2) / yprocs;
   }  
   for (i=numlev-1;i>=0;i--) {
     if ((xpts_per_proc[i] < 2) || (ypts_per_proc[i] < 2)) {
       minlevel = i+1;
       break;
     }
   }    
 
   for (i=0;i<numlev;i++) {
     temp += imx[i];
   }
   temp = 0;
   j = 0;
   for (k=0;k<numlev;k++) {
     for (i=0;i<imx[k];i++) {
       j++;
       temp += jmx[k];
     }
   }

   d_size = nprocs*sizeof(double ***);
   psi = (double ****) valloc(d_size);;
   psim = (double ****) valloc(d_size);;
   work1 = (double ****) valloc(d_size);;
   work4 = (double ****) valloc(d_size);;
   work5 = (double ****) valloc(d_size);;
   work7 = (double ****) valloc(d_size);;
   temparray = (double ****) valloc(d_size);;

   d_size = 2*sizeof(double **);
   for (i=0;i<nprocs;i++) {
     psi[i] = (double ***) valloc(d_size);;
     psim[i] = (double ***) valloc(d_size);;
     work1[i] = (double ***) valloc(d_size);;
     work4[i] = (double ***) valloc(d_size);;
     work5[i] = (double ***) valloc(d_size);;
     work7[i] = (double ***) valloc(d_size);;
     temparray[i] = (double ***) valloc(d_size);;
   }

   d_size = nprocs*sizeof(double **);
   psium = (double ***) valloc(d_size);;
   psilm = (double ***) valloc(d_size);;
   psib = (double ***) valloc(d_size);;
   ga = (double ***) valloc(d_size);;
   gb = (double ***) valloc(d_size);;
   work2 = (double ***) valloc(d_size);;
   work3 = (double ***) valloc(d_size);;
   work6 = (double ***) valloc(d_size);;
   tauz = (double ***) valloc(d_size);;
   oldga = (double ***) valloc(d_size);;
   oldgb = (double ***) valloc(d_size);;

   gp = (struct Global_Private *) valloc((nprocs+1)*sizeof(struct Global_Private));;
   for (i=0;i<nprocs;i++) {
     gp[i].rel_num_x = (int *) valloc(numlev*sizeof(int));;
     gp[i].rel_num_y = (int *) valloc(numlev*sizeof(int));;
     gp[i].eist = (int *) valloc(numlev*sizeof(int));;
     gp[i].ejst = (int *) valloc(numlev*sizeof(int));;
     gp[i].oist = (int *) valloc(numlev*sizeof(int));;
     gp[i].ojst = (int *) valloc(numlev*sizeof(int));;
     gp[i].rlist = (int *) valloc(numlev*sizeof(int));;
     gp[i].rljst = (int *) valloc(numlev*sizeof(int));;
     gp[i].rlien = (int *) valloc(numlev*sizeof(int));;
     gp[i].rljen = (int *) valloc(numlev*sizeof(int));;
     gp[i].multi_time = 0;
     gp[i].total_time = 0;
   }

   subblock();

   x_part = (jm - 2)/xprocs + 2;
   y_part = (im - 2)/yprocs + 2;

   d_size = x_part*y_part*sizeof(double) + y_part*sizeof(double *);

   global = (struct global_struct *) valloc(sizeof(struct global_struct));;  
   for (i=0;i<nprocs;i++) {
     psi[i][0] = (double **) valloc(d_size);;
     psi[i][1] = (double **) valloc(d_size);;
     psim[i][0] = (double **) valloc(d_size);;
     psim[i][1] = (double **) valloc(d_size);;
     psium[i] = (double **) valloc(d_size);;
     psilm[i] = (double **) valloc(d_size);;
     psib[i] = (double **) valloc(d_size);;
     ga[i] = (double **) valloc(d_size);;
     gb[i] = (double **) valloc(d_size);;
     work1[i][0] = (double **) valloc(d_size);;
     work1[i][1] = (double **) valloc(d_size);;
     work2[i] = (double **) valloc(d_size);;
     work3[i] = (double **) valloc(d_size);;
     work4[i][0] = (double **) valloc(d_size);;
     work4[i][1] = (double **) valloc(d_size);;
     work5[i][0] = (double **) valloc(d_size);;
     work5[i][1] = (double **) valloc(d_size);;
     work6[i] = (double **) valloc(d_size);;
     work7[i][0] = (double **) valloc(d_size);;
     work7[i][1] = (double **) valloc(d_size);;
     temparray[i][0] = (double **) valloc(d_size);;
     temparray[i][1] = (double **) valloc(d_size);;
     tauz[i] = (double **) valloc(d_size);;
     oldga[i] = (double **) valloc(d_size);;
     oldgb[i] = (double **) valloc(d_size);;
   }
   f = (double *) valloc(im*sizeof(double));;

   multi = (struct multi_struct *) valloc(sizeof(struct multi_struct));;

   d_size = numlev*sizeof(double **);
   if (numlev%2 == 1) {         /* To make sure that the actual data 
                                   starts double word aligned, add an extra
                                   pointer */
     d_size += sizeof(double **);
   }
   for (i=0;i<numlev;i++) {
     d_size += ((imx[i]-2)/yprocs+2)*((jmx[i]-2)/xprocs+2)*sizeof(double)+
              ((imx[i]-2)/yprocs+2)*sizeof(double *);
   }

   d_size *= nprocs;

   if (nprocs%2 == 1) {         /* To make sure that the actual data 
                                   starts double word aligned, add an extra
                                   pointer */
     d_size += sizeof(double ***);
   }

   d_size += nprocs*sizeof(double ***);
   q_multi = (double ****) valloc(d_size);;
   rhs_multi = (double ****) valloc(d_size);;

   locks = (struct locks_struct *) valloc(sizeof(struct locks_struct));;
   bars = (struct bars_struct *) valloc(sizeof(struct bars_struct));;

   {pthread_mutex_init(&(locks->idlock), NULL);}
   {pthread_mutex_init(&(locks->psiailock), NULL);}
   {pthread_mutex_init(&(locks->psibilock), NULL);}
   {pthread_mutex_init(&(locks->donelock), NULL);}
   {pthread_mutex_init(&(locks->error_lock), NULL);}
   {pthread_mutex_init(&(locks->bar_lock), NULL);}

   {
	unsigned long	Error;

	Error = pthread_mutex_init(&(bars->iteration).mutex, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		exit(-1);
	}

	Error = pthread_cond_init(&(bars->iteration).cv, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		pthread_mutex_destroy(&(bars->iteration).mutex);
		exit(-1);
	}

	(bars->iteration).counter = 0;
	(bars->iteration).cycle = 0;
}
   {
	unsigned long	Error;

	Error = pthread_mutex_init(&(bars->gsudn).mutex, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		exit(-1);
	}

	Error = pthread_cond_init(&(bars->gsudn).cv, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		pthread_mutex_destroy(&(bars->gsudn).mutex);
		exit(-1);
	}

	(bars->gsudn).counter = 0;
	(bars->gsudn).cycle = 0;
}
   {
	unsigned long	Error;

	Error = pthread_mutex_init(&(bars->p_setup).mutex, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		exit(-1);
	}

	Error = pthread_cond_init(&(bars->p_setup).cv, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		pthread_mutex_destroy(&(bars->p_setup).mutex);
		exit(-1);
	}

	(bars->p_setup).counter = 0;
	(bars->p_setup).cycle = 0;
} 
   {
	unsigned long	Error;

	Error = pthread_mutex_init(&(bars->p_redph).mutex, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		exit(-1);
	}

	Error = pthread_cond_init(&(bars->p_redph).cv, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		pthread_mutex_destroy(&(bars->p_redph).mutex);
		exit(-1);
	}

	(bars->p_redph).counter = 0;
	(bars->p_redph).cycle = 0;
} 
   {
	unsigned long	Error;

	Error = pthread_mutex_init(&(bars->p_soln).mutex, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		exit(-1);
	}

	Error = pthread_cond_init(&(bars->p_soln).cv, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		pthread_mutex_destroy(&(bars->p_soln).mutex);
		exit(-1);
	}

	(bars->p_soln).counter = 0;
	(bars->p_soln).cycle = 0;
} 
   {
	unsigned long	Error;

	Error = pthread_mutex_init(&(bars->p_subph).mutex, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		exit(-1);
	}

	Error = pthread_cond_init(&(bars->p_subph).cv, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		pthread_mutex_destroy(&(bars->p_subph).mutex);
		exit(-1);
	}

	(bars->p_subph).counter = 0;
	(bars->p_subph).cycle = 0;
} 
   {
	unsigned long	Error;

	Error = pthread_mutex_init(&(bars->sl_prini).mutex, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		exit(-1);
	}

	Error = pthread_cond_init(&(bars->sl_prini).cv, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		pthread_mutex_destroy(&(bars->sl_prini).mutex);
		exit(-1);
	}

	(bars->sl_prini).counter = 0;
	(bars->sl_prini).cycle = 0;
}
   {
	unsigned long	Error;

	Error = pthread_mutex_init(&(bars->sl_psini).mutex, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		exit(-1);
	}

	Error = pthread_cond_init(&(bars->sl_psini).cv, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		pthread_mutex_destroy(&(bars->sl_psini).mutex);
		exit(-1);
	}

	(bars->sl_psini).counter = 0;
	(bars->sl_psini).cycle = 0;
}
   {
	unsigned long	Error;

	Error = pthread_mutex_init(&(bars->sl_onetime).mutex, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		exit(-1);
	}

	Error = pthread_cond_init(&(bars->sl_onetime).cv, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		pthread_mutex_destroy(&(bars->sl_onetime).mutex);
		exit(-1);
	}

	(bars->sl_onetime).counter = 0;
	(bars->sl_onetime).cycle = 0;
}
   {
	unsigned long	Error;

	Error = pthread_mutex_init(&(bars->sl_phase_1).mutex, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		exit(-1);
	}

	Error = pthread_cond_init(&(bars->sl_phase_1).cv, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		pthread_mutex_destroy(&(bars->sl_phase_1).mutex);
		exit(-1);
	}

	(bars->sl_phase_1).counter = 0;
	(bars->sl_phase_1).cycle = 0;
}
   {
	unsigned long	Error;

	Error = pthread_mutex_init(&(bars->sl_phase_2).mutex, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		exit(-1);
	}

	Error = pthread_cond_init(&(bars->sl_phase_2).cv, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		pthread_mutex_destroy(&(bars->sl_phase_2).mutex);
		exit(-1);
	}

	(bars->sl_phase_2).counter = 0;
	(bars->sl_phase_2).cycle = 0;
}
   {
	unsigned long	Error;

	Error = pthread_mutex_init(&(bars->sl_phase_3).mutex, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		exit(-1);
	}

	Error = pthread_cond_init(&(bars->sl_phase_3).cv, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		pthread_mutex_destroy(&(bars->sl_phase_3).mutex);
		exit(-1);
	}

	(bars->sl_phase_3).counter = 0;
	(bars->sl_phase_3).cycle = 0;
}
   {
	unsigned long	Error;

	Error = pthread_mutex_init(&(bars->sl_phase_4).mutex, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		exit(-1);
	}

	Error = pthread_cond_init(&(bars->sl_phase_4).cv, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		pthread_mutex_destroy(&(bars->sl_phase_4).mutex);
		exit(-1);
	}

	(bars->sl_phase_4).counter = 0;
	(bars->sl_phase_4).cycle = 0;
}
   {
	unsigned long	Error;

	Error = pthread_mutex_init(&(bars->sl_phase_5).mutex, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		exit(-1);
	}

	Error = pthread_cond_init(&(bars->sl_phase_5).cv, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		pthread_mutex_destroy(&(bars->sl_phase_5).mutex);
		exit(-1);
	}

	(bars->sl_phase_5).counter = 0;
	(bars->sl_phase_5).cycle = 0;
}
   {
	unsigned long	Error;

	Error = pthread_mutex_init(&(bars->sl_phase_6).mutex, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		exit(-1);
	}

	Error = pthread_cond_init(&(bars->sl_phase_6).cv, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		pthread_mutex_destroy(&(bars->sl_phase_6).mutex);
		exit(-1);
	}

	(bars->sl_phase_6).counter = 0;
	(bars->sl_phase_6).cycle = 0;
}
   {
	unsigned long	Error;

	Error = pthread_mutex_init(&(bars->sl_phase_7).mutex, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		exit(-1);
	}

	Error = pthread_cond_init(&(bars->sl_phase_7).cv, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		pthread_mutex_destroy(&(bars->sl_phase_7).mutex);
		exit(-1);
	}

	(bars->sl_phase_7).counter = 0;
	(bars->sl_phase_7).cycle = 0;
}
   {
	unsigned long	Error;

	Error = pthread_mutex_init(&(bars->sl_phase_8).mutex, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		exit(-1);
	}

	Error = pthread_cond_init(&(bars->sl_phase_8).cv, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		pthread_mutex_destroy(&(bars->sl_phase_8).mutex);
		exit(-1);
	}

	(bars->sl_phase_8).counter = 0;
	(bars->sl_phase_8).cycle = 0;
}
   {
	unsigned long	Error;

	Error = pthread_mutex_init(&(bars->sl_phase_9).mutex, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		exit(-1);
	}

	Error = pthread_cond_init(&(bars->sl_phase_9).cv, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		pthread_mutex_destroy(&(bars->sl_phase_9).mutex);
		exit(-1);
	}

	(bars->sl_phase_9).counter = 0;
	(bars->sl_phase_9).cycle = 0;
}
   {
	unsigned long	Error;

	Error = pthread_mutex_init(&(bars->sl_phase_10).mutex, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		exit(-1);
	}

	Error = pthread_cond_init(&(bars->sl_phase_10).cv, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		pthread_mutex_destroy(&(bars->sl_phase_10).mutex);
		exit(-1);
	}

	(bars->sl_phase_10).counter = 0;
	(bars->sl_phase_10).cycle = 0;
}
   {
	unsigned long	Error;

	Error = pthread_mutex_init(&(bars->error_barrier).mutex, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		exit(-1);
	}

	Error = pthread_cond_init(&(bars->error_barrier).cv, NULL);
	if (Error != 0) {
		printf("Error while initializing barrier.\n");
		pthread_mutex_destroy(&(bars->error_barrier).mutex);
		exit(-1);
	}

	(bars->error_barrier).counter = 0;
	(bars->error_barrier).cycle = 0;
}

   link_all();

   multi->err_multi = 0.0;
   i_int_coeff[0] = 0.0;
   j_int_coeff[0] = 0.0;
   for (i=0;i<numlev;i++) {
     i_int_coeff[i] = 1.0/(imx[i]-1);
     j_int_coeff[i] = 1.0/(jmx[i]-1);
   }

/* initialize constants and variables

   id is a global shared variable that has fetch-and-add operations
   performed on it by processes to obtain their pids.   */

   global->id = 0;
   global->psibi = 0.0;
   pi = atan(1.0);
   pi = 4.*pi;

   factjacob = -1./(12.*res*res);
   factlap = 1./(res*res);
   eig2 = -h*f0*f0/(h1*h3*gpr);

   jmm1 = jm-1 ;
   ysca = ((double) jmm1)*res ;

   im = (imx[numlev-1]-2)/yprocs + 2;
   jm = (jmx[numlev-1]-2)/xprocs + 2;

   MEMSYS_ON

   for (i=1;i<nprocs;i++) {
     {
	if (ParmacsThreadNum >= PARMACS_MAX_THREADS) {
		printf("Increase PARMACS_MAX_THREADS(%u) !\n", PARMACS_MAX_THREADS);
		exit(-1);
	}
	pthread_create(&ParmacsThreads[ParmacsThreadNum++], NULL, (void * (*)(void *))slave, NULL);
}  
   }

   if (do_output) {
     printf("                       MULTIGRID OUTPUTS\n");
   }

   slave();
   {
	unsigned long	i, Error;
	for (i = 0; i < ParmacsThreadNum; i++) {
		Error = pthread_join(ParmacsThreads[i], NULL);
		if (Error != 0) {
			printf("Error in pthread_join().\n");
			exit(-1);
		}
	}
}
   {
	struct timeval	FullTime;

	gettimeofday(&FullTime, NULL);
	(computeend) = (unsigned long)(FullTime.tv_usec + FullTime.tv_sec * 1000000);
}

   MEMSYS_OFF

   printf("\n");
   printf("                       PROCESS STATISTICS\n");
   printf("                  Total          Multigrid         Multigrid\n");
   printf(" Proc             Time             Time            Fraction\n");
   printf("    0   %15.0f    %15.0f        %10.3f\n",
          gp[0].total_time,gp[0].multi_time,
          gp[0].multi_time/gp[0].total_time);

   if (do_stats) {
     min_total = max_total = avg_total = gp[0].total_time;
     min_multi = max_multi = avg_multi = gp[0].multi_time;
     min_frac = max_frac = avg_frac = gp[0].multi_time/gp[0].total_time;
     for (i=1;i<nprocs;i++) {
       if (gp[i].total_time > max_total) {
         max_total = gp[i].total_time;
       }
       if (gp[i].total_time < min_total) {
         min_total = gp[i].total_time;
       }
       if (gp[i].multi_time > max_multi) {
         max_multi = gp[i].multi_time;
       }
       if (gp[i].multi_time < min_multi) {
         min_multi = gp[i].multi_time;
       }
       if (gp[i].multi_time/gp[i].total_time > max_frac) {
         max_frac = gp[i].multi_time/gp[i].total_time;
       }
       if (gp[i].multi_time/gp[i].total_time < min_frac) {
         min_frac = gp[i].multi_time/gp[i].total_time;
       }
       avg_total += gp[i].total_time;
       avg_multi += gp[i].multi_time;
       avg_frac += gp[i].multi_time/gp[i].total_time;
     }
     avg_total = avg_total / nprocs;
     avg_multi = avg_multi / nprocs;
     avg_frac = avg_frac / nprocs;
     for (i=1;i<nprocs;i++) {
       printf("  %3d   %15.0f    %15.0f        %10.3f\n",
	      i,gp[i].total_time,gp[i].multi_time,
	      gp[i].multi_time/gp[i].total_time);
     }
     printf("  Avg   %15.0f    %15.0f        %10.3f\n",
            avg_total,avg_multi,avg_frac);
     printf("  Min   %15.0f    %15.0f        %10.3f\n",
            min_total,min_multi,min_frac);
     printf("  Max   %15.0f    %15.0f        %10.3f\n",
            max_total,max_multi,max_frac);
   }
   printf("\n");

   global->starttime = start;
   printf("                       TIMING INFORMATION\n");
   printf("Start time                        : %16d\n",
           global->starttime);
   printf("Initialization finish time        : %16d\n",
           global->trackstart);
   printf("Overall finish time               : %16d\n",
           computeend);
   printf("Total time with initialization    : %16d\n",
           computeend-global->starttime);
   printf("Total time without initialization : %16d\n",
           computeend-global->trackstart);
   printf("    (excludes first timestep)\n");
   printf("\n");

   {exit(0);}
}

int log_2(number)

int number;

{
  int cumulative = 1;
  int out = 0;
  int done = 0;

  while ((cumulative < number) && (!done) && (out < 50)) {
    if (cumulative == number) {
      done = 1;
    } else {
      cumulative = cumulative * 2;
      out ++;
    }
  }

  if (cumulative == number) {
    return(out);
  } else {
    return(-1);
  }
}

void printerr(s)

char *s;

{
  fprintf(stderr,"ERROR: %s\n",s);
}

