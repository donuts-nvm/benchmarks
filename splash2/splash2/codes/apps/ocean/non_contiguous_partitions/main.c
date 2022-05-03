
/*************************************************************************/
/*                                                                       */
/*  SPLASH Ocean Code                                                    */
/*                                                                       */
/*  This application studies the role of eddy and boundary currents in   */
/*  influencing large-scale ocean movements.  This implementation uses   */
/*  statically allocated two-dimensional arrays for grid data storage.   */
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


#include <stdio.h>
#include <math.h>
#include <time.h>
#define DEFAULT_N      258
#define DEFAULT_P        1
#define DEFAULT_E        1e-7
#define DEFAULT_T    28800.0
#define DEFAULT_R    20000.0
#define INPROCS         64         /* Maximum number of processors */
#define IMAX          1026
#define JMAX          1026
#define MAX_LEVELS      11
#define PAGE_SIZE     4096

struct global_struct {
   int id;
   int starttime;
   int trackstart;
   double psiai;
   double psibi;
} *global;

struct fields_struct {
   double psi[2][IMAX][JMAX];
   double psim[2][IMAX][JMAX];
} *fields;
  
struct fields2_struct {
   double psium[IMAX][JMAX];
   double psilm[IMAX][JMAX];
} *fields2;

struct wrk1_struct {
   double psib[IMAX][JMAX];
   double ga[IMAX][JMAX];
   double gb[IMAX][JMAX];
} *wrk1;

struct wrk3_struct {
   double work1[2][IMAX][JMAX];
   double work2[IMAX][JMAX];
} *wrk3;

struct wrk2_struct {
   double work3[IMAX][JMAX];
   double f[IMAX];
} *wrk2;

struct wrk4_struct {
   double work4[2][IMAX][JMAX];
   double work5[2][IMAX][JMAX];
} *wrk4;

struct wrk6_struct {
   double work6[IMAX][JMAX];
} *wrk6;

struct wrk5_struct {
   double work7[2][IMAX][JMAX];
   double temparray[2][IMAX][JMAX];
} *wrk5;

struct frcng_struct {
   double tauz[IMAX][JMAX];
} *frcng;

struct iter_struct {
   int notdone;
   double work8[IMAX][JMAX];
   double work9[IMAX][JMAX];
} *iter;

struct guess_struct {
   double oldga[IMAX][JMAX];
   double oldgb[IMAX][JMAX];
} *guess;

struct multi_struct {
   double q_multi[MAX_LEVELS][IMAX][JMAX];
   double rhs_multi[MAX_LEVELS][IMAX][JMAX];
   double err_multi;
   int numspin;
   int spinflag[INPROCS];
} *multi;

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

int startcol[2][INPROCS];
int nprocs = DEFAULT_P;
int startrow[2][INPROCS];
double h1 = 1000.0;
double h3 = 4000.0;
double h = 5000.0;
double lf = -5.12e11;
double eps = 0;
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
int minlev;
int imx[MAX_LEVELS];
int jmx[MAX_LEVELS];
double lev_res[MAX_LEVELS];
double lev_tol[MAX_LEVELS];
double maxwork = 10000.0;

struct Global_Private {
  char pad[PAGE_SIZE];
  double multi_time;
  double total_time;
  int rel_start_x[MAX_LEVELS];
  int rel_start_y[MAX_LEVELS];
  int rel_num_x[MAX_LEVELS];
  int rel_num_y[MAX_LEVELS];
  int eist[MAX_LEVELS];
  int ejst[MAX_LEVELS];
  int oist[MAX_LEVELS];
  int ojst[MAX_LEVELS];
  int eiest[MAX_LEVELS];
  int ejest[MAX_LEVELS];
  int oiest[MAX_LEVELS];
  int ojest[MAX_LEVELS];
  int rlist[MAX_LEVELS];
  int rljst[MAX_LEVELS];
  int rlien[MAX_LEVELS];
  int rljen[MAX_LEVELS];
  int iist[MAX_LEVELS];
  int ijst[MAX_LEVELS];
  int iien[MAX_LEVELS];
  int ijen[MAX_LEVELS];
  int pist[MAX_LEVELS];
  int pjst[MAX_LEVELS];
  int pien[MAX_LEVELS];
  int pjen[MAX_LEVELS];
} *gp;

double i_int_coeff[MAX_LEVELS];
double j_int_coeff[MAX_LEVELS];
int xprocs;
int yprocs;
int do_stats = 0;
int do_output = 0;

void main(argc, argv)

int argc;
char *argv[];

{
   double s;
   double st2;
   int i;
   int j;
   int xextra;
   int xportion;
   int yextra;
   int yportion;
   int lower;
   double procsqrt;
   int k;
   int logtest;
   double work_multi;
   int my_num;
   unsigned int computeend;
   double min_total;
   double max_total;
   double avg_total;
   double min_multi;
   double max_multi;
   double avg_multi;
   double min_frac;
   double max_frac;
   double avg_frac;
   extern char *optarg;
   int ch;
   unsigned int start;

   {
	struct timeval	FullTime;

	gettimeofday(&FullTime, NULL);
	(start) = (unsigned long)(FullTime.tv_usec + FullTime.tv_sec * 1000000);
}

   while ((ch = getopt(argc, argv, "n:p:e:r:t:soh")) != -1) {
     switch(ch) {
     case 'n': im = atoi(optarg);
               if (im > IMAX) {
                 printerr("Max grid size exceeded\n");
                 exit(-1);
               }
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

   logtest = im-2;
   numlev = 1;
   while (logtest != 1) {
     if (logtest%2 != 0) {
       printerr("Cannot determine number of multigrid levels\n");
       exit(-1);
     }
     logtest = logtest / 2;
     numlev++;
   }

   if (numlev > MAX_LEVELS) {
     printerr("Max grid levels exceeded for multigrid\n");
     exit(-1);
   }

   jm = im;
   printf("\n");
   printf("Ocean simulation with W-cycle multigrid solver\n");
   printf("    Processors                         : %1d\n",nprocs);
   printf("    Grid size                          : %1d x %1d\n",im,jm);
   printf("    Grid resolution (meters)           : %0.2f\n",res);
   printf("    Time between relaxations (seconds) : %0.0f\n",dtau);
   printf("    Error tolerance                    : %0.7g\n",tolerance);
   printf("\n");

   gp = (struct Global_Private *) valloc((nprocs+1)*sizeof(struct Global_Private));;
   for (i=0;i<nprocs;i++) {
     gp[i].multi_time = 0;
     gp[i].total_time = 0;
   }
   global = (struct global_struct *) valloc(sizeof(struct global_struct));;
   fields = (struct fields_struct *) valloc(sizeof(struct fields_struct));;
   fields2 = (struct fields2_struct *) valloc(sizeof(struct fields2_struct));;
   wrk1 = (struct wrk1_struct *) valloc(sizeof(struct wrk1_struct));;
   wrk3 = (struct wrk3_struct *) valloc(sizeof(struct wrk3_struct));;
   wrk2 = (struct wrk2_struct *) valloc(sizeof(struct wrk2_struct));;
   wrk4 = (struct wrk4_struct *) valloc(sizeof(struct wrk4_struct));;
   wrk6 = (struct wrk6_struct *) valloc(sizeof(struct wrk6_struct));;
   wrk5 = (struct wrk5_struct *) valloc(sizeof(struct wrk5_struct));;
   frcng = (struct frcng_struct *) valloc(sizeof(struct frcng_struct));;
   iter = (struct iter_struct *) valloc(sizeof(struct iter_struct));;
   guess = (struct guess_struct *) valloc(sizeof(struct guess_struct));;
   multi = (struct multi_struct *) valloc(sizeof(struct multi_struct));;
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

   imx[numlev-1] = im;
   jmx[numlev-1] = jm;
   lev_res[numlev-1] = res;
   lev_tol[numlev-1] = tolerance;
   multi->err_multi = 0.0;
   multi->numspin = 0;
   for (i=0;i<nprocs;i++) {
     multi->spinflag[i] = 0;
   }

   for (i=numlev-2;i>=0;i--) {
     imx[i] = ((imx[i+1] - 2) / 2) + 2;
     jmx[i] = ((jmx[i+1] - 2) / 2) + 2;
     lev_res[i] = lev_res[i+1] * 2;
   }

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

/* Determine starting coord and number of points to process in     */
/* each direction                                                  */

   for (i=0;i<numlev;i++) {
     xportion = (jmx[i] - 2) / xprocs;
     xextra = (jmx[i] - 2) % xprocs;
     for (j=0;j<xprocs;j++) {
       if (xextra == 0) {
         for (k=0;k<yprocs;k++) {
           gp[k*xprocs+j].rel_start_x[i] = j * xportion + 1;
           gp[k*xprocs+j].rel_num_x[i] = xportion;
         }
       } else {
         if (j + 1 > xextra) {
           for (k=0;k<yprocs;k++) {
             lower = xextra * (xportion + 1);
             gp[k*xprocs+j].rel_start_x[i] = lower + (j - xextra) * xportion + 1;
             gp[k*xprocs+j].rel_num_x[i] = xportion;
           }
         } else {
           for (k=0;k<yprocs;k++) {
             gp[k*xprocs+j].rel_start_x[i] = j * (xportion + 1) + 1;
             gp[k*xprocs+j].rel_num_x[i] = xportion + 1;
           }
         }
       }
     }
     yportion = (imx[i] - 2) / yprocs;
     yextra = (imx[i] - 2) % yprocs;
     for (j=0;j<yprocs;j++) {
       if (yextra == 0) {
         for (k=0;k<xprocs;k++) {
           gp[j*xprocs+k].rel_start_y[i] = j * yportion + 1;
           gp[j*xprocs+k].rel_num_y[i] = yportion;
         }
       } else {
         if (j + 1 > yextra) {
           for (k=0;k<xprocs;k++) {
             lower = yextra * (yportion + 1);
             gp[j*xprocs+k].rel_start_y[i] = lower + (j - yextra) * yportion + 1;
             gp[j*xprocs+k].rel_num_y[i] = yportion;
           }
         } else {
           for (k=0;k<xprocs;k++) {
             gp[j*xprocs+k].rel_start_y[i] = j * (yportion + 1) + 1;
             gp[j*xprocs+k].rel_num_y[i] = yportion + 1;
           }
         }
       }
     }
   }

   i_int_coeff[0] = 0.0;
   j_int_coeff[0] = 0.0;
   for (i=0;i<numlev;i++) {
     i_int_coeff[i] = 1.0/(imx[i]-1);
     j_int_coeff[i] = 1.0/(jmx[i]-1);
   }

   for (my_num=0;my_num<nprocs;my_num++) {
     for (i=0;i<numlev;i++) {
       gp[my_num].rlist[i] = gp[my_num].rel_start_y[i];
       gp[my_num].rljst[i] = gp[my_num].rel_start_x[i];
       gp[my_num].rlien[i] = gp[my_num].rlist[i] + gp[my_num].rel_num_y[i] - 1;
       gp[my_num].rljen[i] = gp[my_num].rljst[i] + gp[my_num].rel_num_x[i] - 1;
       gp[my_num].iist[i] = gp[my_num].rel_start_y[i];
       gp[my_num].ijst[i] = gp[my_num].rel_start_x[i];
       gp[my_num].iien[i] = gp[my_num].iist[i] + gp[my_num].rel_num_y[i] - 1;
       gp[my_num].ijen[i] = gp[my_num].ijst[i] + gp[my_num].rel_num_x[i] - 1;
       gp[my_num].pist[i] = gp[my_num].rel_start_y[i];
       gp[my_num].pjst[i] = gp[my_num].rel_start_x[i];
       gp[my_num].pien[i] = gp[my_num].pist[i] + gp[my_num].rel_num_y[i] - 1;
       gp[my_num].pjen[i] = gp[my_num].pjst[i] + gp[my_num].rel_num_x[i] - 1;
  
       if (gp[my_num].pist[i] == 1) {
         gp[my_num].pist[i] = 0;
       }
       if (gp[my_num].pjst[i] == 1) {
         gp[my_num].pjst[i] = 0;
       }
       if (gp[my_num].pien[i] == imx[i] - 2) {
         gp[my_num].pien[i] = imx[i]-1;
       }
       if (gp[my_num].pjen[i] == jmx[i] - 2) {
         gp[my_num].pjen[i] = jmx[i]-1;
       }

       if (gp[my_num].rlist[i] % 2 == 0) {
         gp[my_num].eist[i] = gp[my_num].rlist[i];
         gp[my_num].oist[i] = gp[my_num].rlist[i] + 1;
       } else {
         gp[my_num].eist[i] = gp[my_num].rlist[i] + 1;
         gp[my_num].oist[i] = gp[my_num].rlist[i];
       }
       if (gp[my_num].rljst[i] % 2 == 0) {
         gp[my_num].ejst[i] = gp[my_num].rljst[i];
         gp[my_num].ojst[i] = gp[my_num].rljst[i] + 1;
       } else {
         gp[my_num].ejst[i] = gp[my_num].rljst[i] + 1;
         gp[my_num].ojst[i] = gp[my_num].rljst[i];
       }
       if (gp[my_num].rlien[i] == imx[i]-2) {
         gp[my_num].rlien[i] = gp[my_num].rlien[i] - 1;
         if (gp[my_num].rlien[i] % 2 == 0) {
           gp[my_num].ojest[i] = gp[my_num].ojst[i];
           gp[my_num].ejest[i] = gp[my_num].ejst[i];
         } else {
           gp[my_num].ojest[i] = gp[my_num].ejst[i];
           gp[my_num].ejest[i] = gp[my_num].ojst[i];
         }
       }
       if (gp[my_num].rljen[i] == jmx[i]-2) {
         gp[my_num].rljen[i] = gp[my_num].rljen[i] - 1;
         if (gp[my_num].rljen[i] % 2 == 0) {
           gp[my_num].oiest[i] = gp[my_num].oist[i];
           gp[my_num].eiest[i] = gp[my_num].eist[i];
         } else {
           gp[my_num].oiest[i] = gp[my_num].eist[i];
           gp[my_num].eiest[i] = gp[my_num].oist[i];
         }
       }
     }
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
   for (i=0;i<im;i++) {
     for (j=0;j<jm;j++) {
       guess->oldga[i][j] = 0.0;
       guess->oldgb[i][j] = 0.0;
     }
   }

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

