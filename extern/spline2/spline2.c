#define  VERSION "6.1"
/*
 *	Spline2/Supersp
 *
 *      Public version (5.02) of 14 may 1997
 *      Update to version (5.03), 2 october 2001
 *               Changed REJLEV into a command-line option -L for fine tuning.
 *      Update to version (5.1), 16 october 2001
 *               Introduced -K option to specify assumed correlation length.
 *      Update to version (5.11), 26 october 2001
 *               Introduced -e option to allow "equal-information" splines,
 *               i.e. splines with non-optimized breakpoints. By default,
 *               these are now no longer allowed.
 *      Update to version (6.0), 22 january 2002
 *               Automatic search for autocorrelation in noise: -s option.
 *		         New output file: splacf.
 *		         New output file: splksi.
 *               Option -e inverted: forbid "equal-information" splines.
 *               Option -F now redundant. Full output is now standard.
 *               File splstat is nonsense for -s.
 *               Function lgamma (from Marcus Karolewski) added to code.
 *               Output file splsum contains a five-lines summary of results.
 *      Update to version (6.0a), 25 january 2002
 *		 Ksi steps over a finer mesh as long as it is < dxav.
 *      Update to version (6.1), 14 october 2004
 *               No functional change. Mainly: fixed array dimensions and
 *               array overflow checking, and improved help text.
 *
 ****   Info:
 *
 *      For information about splines and the algorithm-kernels used, see
 *	(Ref. 1) Carl de Boor, "A Practical Guide to Splines",
 *		 (Springer, 1978).
 *
 *	The curve-fitting algorithms and strategies employed are described in:
 *      (Ref. 2) Barend J. Thijsse, Mark A. Hollanders, and J. Hendrikse,
 *               "A practical algorithm for least-squares spline approximation
 *               of data containing noise", Computers In Physics,
 *               Jul/Aug (1998).
 *      A paper explaining the autocorrelation search method is in preparation.
 *
 *	Authors and owners of this program are:
 *		Barend J. Thijsse and Mark A. Hollanders
 *              Virtual Materials Laboratory
 *		Department of Materials Science and Engineering
 *		Delft University of Technology
 *		Rotterdamseweg 137, 2628 AL  Delft
 *		Netherlands
 *			Phone: +31 15 278 2221
 *			Fax:   +31 15 278 6730
 *			E-mail: B.J.Thijsse@tnw.tudelft.nl
 *			URL: http://dutsm183.stm.tudelft.nl/fcm
 *
 *	Additional Acknowledgments to:
 *		- Mathsoft Engineering & Education, Inc.
 *		- Jan Hendrikse
 *		- Marcus Karolewski
 */

/* ---INCLUDES--- */
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <math.h>

/* ---DEFINES (NUMBERS)--- */
/* v6.1 Array dimensioning fixed */
/* --------------------------------------------------------------------------*/
#define NDATMAX 10000	/* Maximum number of data points (ntau=1..NDATMAX) */
#define LMAX 2000	/* Maximum number of intervals (l) */
#define KMAX 24		/* Maximum spline order (k) */
#define RMAX 4000	/* Maximum number of intermediate results 
			   (Overflow of 'iresul' and 'resul' NOT TESTED) */
#define KSIMAX  200     /* Maximum number of ksi values tested */
#define ACFMAX 24	/* Maximum no. of autocorr.fct. points (njan=1..ACFMAX) */
/* --------------------------------------------------------------------------*/

#define NMAX (KMAX+LMAX-1)	/* Maximum spline dimension (n) */
#define NTMAX (2*NMAX)		/* Maximum number of knots */
#define NBRMAX (LMAX+1)		/* Maximum number of breakpoints */

#define UPFACTOR  1.1   /* Increment factor for l */
#define DOWNFACTOR 0.95 /* Decrement factor for l */
#define MAXHYPFRAC 0.5  /* Max l value (fraction of N) for hyperspline */
#define UPMARGIN 1e-7   /* Calculated (double) values of l this close
			   under an integer are rounded upward instead
			   of truncated downward (v. 5.02) */
#define ACFRANGE1 3.0   /* The autocorr.fct. of the residuals is tested for.. */
#define ACFRANGE2 3     /* ..njan = 1 to ACFRANGE1*(ksi/dxav)+ACFRANGE2 */

/* ---DEFINES (MACROS)--- */
#define TRUNC_SPEC(i,d) (((double)((i)+1)-(d)) < UPMARGIN ? ((i)+1) : (i))

/* ---ANNOUNCE FUNCTIONS---  */

void l2knts(double* ara,int* iptr,int* jptr,double* arb,int* kptr);
void l2sub(int *iptr, int iopt, int wr);
void newknt(double ara[],double dara[][NMAX+1],int* iptr,int* jptr,
	    double arb[],int* kptr, double darb[][LMAX+1]);
void l2appr(double ara[],int* iptr,int* jptr,double dara[][NMAX+1],
	    double arb[],double arc[]);
void bchfac(double dara[][NMAX+1],int* iptr,int* jptr,double  ara[]);
void bchslv(double dara[][NMAX+1],int* iptr,int* jptr,double  ara[]);
void bsplpp(double ara[],double arb[],int* iptr,int* jptr,double dara[][NMAX+1],
	    double arc[], double darb[][NMAX+1],int* kptr);
void bsplvb(double ara[],int jhigh,int index,double* xptr,
	    int* iptr, double arb[]);
void l2err(int prfun, int grfun);
void interv(double ara[],int* iptr,double* xptr,int* jptr,int* kptr);
void dwtest(double x,double* xptr,double* yptr);
void gser(double* xptr,double a,double x,double* yptr);
void gcf(double* xptr,double a,double x,double* yptr);


double ppvalu(double ara[],double dara[][NMAX+1],
	      int* iptr,int* jptr,double* xptr,int jderiv); 
double chitest(double chi,int fr);
double betai(double a, double b, double x);
double betacf(double a,double b,double x);
double acffunc(int index,double deltax,double ksi);
double lgamma(double xx);

/*----GLOBAL VARIABLES----*/
int    km1;		/* degree of polynomials */
int    k;		/* order of polynomials */
int    ntau;		/* number of datapoints */
int    l;
int    lnew;		/* number of intervals */
int    ibeg;		/* starting index of knot optimization */
int    n;		/* order of spline-approximation = k+l-1 */
int    freed;		/* degrees of freedom left */
int    nsing;		/* number of singularities */
int    ihi; 
int    ilo = 1;	/* flags for function interv() */
int    nl2sub; 		/* index of highest-l non-optimized approximation */
int    il2sub;		/* counter of and number of approximations */
int    isug;		/* index of suggested approximation */
int    iksisug;		/* ksi-index of suggested approximation */
int    logtrans;	/* flag for log10-transformation of x-scale */
int    interactive;	/* flag for the -q option (and isatty) */
int    quite;           /* flag for the -q option */
int    allownonopt;     /* flag for allowing non-optimized breakpoints */
int    jbsp;		/* counter for function bsplvb() */
int    lend; 
int    lbeg;	/* endpoints of interval for sigma determination */
int    lfin;		/* final (?) choice */
int    left;		/* counter for function newknt() */
int    wind;		/* flag for indecisive are warning status */
int    windbp;		/* flag for non-optimized breakpoints warning status */
int    sigma;		/* if =0: sigmas specified in input-file; otherwise =1*/
int    fullon;		/* flag, =1 if full output is required */
int    fix;		/* flag, =1 if sigma is fixed */
int    rel;		/* flag, =1 if sigma is relative to y */
int    done;            /* flag, =1 if Done? has been answered by yes */
int    njan;		/* durbin-watson distance index */
int    nmin; 
int    nmax;	/* minimum and maximum durbin-watson distance index */
int    nset; 
int    ksiset;	/* flags to indicate if n or ksi are specified */
int    acfindex;        /* determines type of assumed autocorr. function */
int    acfsearch;       /* flag; =1 if autocorrelation is automatically searched */
int    iksi;            /* counter for ksi trials */
int    nksi;		/* number of ksi trials */
int    iresul[RMAX+1][3];	/* l and freed for all approximations */
int    iresulksi[KSIMAX+1][7];	/* array to store results for ksi-trial */

double ppvalue;		    /* return value of program ppvalu() */
double    rmsmin, rmsmax;   /* minimum and maximum rms-value */
double    taumin, taumax;   /* minimum and maximum y-value of datapoints */
double    dermax;	    /* maximum absolute value of the spline derivative */
double    dxav;	            /* average point-to-point spacing along x-axis */
double    totalw;	    /* sum of weight factors */
double    rms;		/* root mean square error */
double    dws;		/* durbin-watson statistic(s), in CIP paper called Q */
double    ctest, ltest;	/* percentage points of various distributions */
double    utest, test;
double    rejlev;          /* Rejection level of statistical tests */
double    fixval;		/* (if positive) value at which sigma is fixed */
double    nonsig;		/* value of first nonsignificant digit range */
double    ksi;             /* v5.1 assumed correlation length */
double    ksibegin;        /* start of automatic ksi-range */
double    ksistep;         /* ksi step */
double    ksiend;         /* ksi end */
double    acfdevmin;	/* minimum deviation from assumed acf */
double    acfdevmax;	/* maximum deviation from assumed acf */
double    acffit;		/* overall deviation from assumed acf */
double    tau[NDATMAX+1]; 	/* x-values of input data */
double    gtau[NDATMAX+1];	/* y-values of input data */
double    weight[NDATMAX+1];	/* weight factors of input data */
double    maxweight;       /* largest weight (only used for negative fixval) */
double    ftau[NDATMAX+1];	/* spline approximation data S(x) at input x-values */
double    q[NDATMAX+1];		/* normalized errors */
double    deriv[NDATMAX+1];	/* first derivative of spline approximation */
double    brek[NBRMAX+1];	/* breakpoints */
double    bcoef[NMAX+1];	/* b-coefficients */
double    coef[KMAX+1][NMAX+1];	/* pp-representation */
double    resul[RMAX+1][8];	/* test statistics for all splines */
double    resulksi[KSIMAX+1][6]; /* array to store results for ksi-trial */
double    acfa[ACFMAX+1];     /* values of actual autocorr.fct. of residuals */
double    acfv[ACFMAX+1];     /* values of assumed autocorr.fct. of residuals */
double    deltal[KMAX+1];	/* used in function bsplvb() */
double    deltar[KMAX+1];	/* used in function bsplvb() */

FILE* fp_out = NULL;
FILE* fp_in = NULL;

FILE* fpr = NULL;
FILE* fpp = NULL;
FILE* fpc = NULL;
FILE* fpa = NULL;
FILE* fpk = NULL;
FILE* fpu = NULL;

void usage(void)
{
    printf(".............. spline2 version %s ...............\n", VERSION);
    printf("Usage: spline2 [datafile] [spline_options] -q > /dev/null\n");
    printf("   or: spline2 [datafile] [spline_options] -q | evalsp [evalsp_options]\n");
    printf("   or: spline2 [datafile] [spline_options]\n");
    printf("\n");
    printf("spline_options:\n");
    printf("[-s]           (Best) Perform on-the-fly search for autocorrelation\n");
    printf("                 Do not use -n or -K together with -s\n");
    printf("[-q]           (Best) Quiet mode. Throw away output or pipe to 'evalsp'\n");
    printf("[-i index]     Set index for assumed autocorrelation function:\n");
    printf("                 1=exp(default), 2=gauss, 3=linear, 4=sinc\n");
    printf("[-x xbeg xend] Fit only data between these x-values\n");
    printf("[-X xbeg xend] Ignore data between these x-values (Can be combined with -x)\n");
    printf("[-K ksi]       Assume a specific autocorrelation length\n");
    printf("[-n spacing]   Measure autocorrelation only in data spaced > 1 index apart\n");
    printf("[-a val]       Impose absolute std.dev. s of y-errors (chi2-test)\n");
    printf("                 val>0: s=val, val=-n: n sign. dig., val=0: 3rd col\n");
    printf("[-r val]       Impose relative std.dev. s of y-errors (chi2-test)\n");
    printf("                 val>0: s=val*|y|, val<0: s=|val|*sqrt(|y|)\n");
    printf("\n");
    printf("[-e]           Reject splines with non-optimized breakpoints\n");
    printf("[-k order]     Use splines of order other than 4 (i.e., degree other than 3)\n");
    printf("[-O lopt]      Force knot optimization to start at lopt knots\n");
    printf("[-L rejlev]    Use rejection level of statistical tests other than 0.05\n");
    printf("[-l]	       (Hardly useful) First take 10log of x\n");
    printf("[-o file]      splrep file\n");
    printf("\n");
    printf("Output files of spline2: \n");
    printf("'splsum' : Small report summarizing the main results\n");
    printf("'splrep' : order(k), #intvls(l), l+1 brkpnts, l+k-1 spline coefficients\n");
    printf("'splacf' : n, <d[i+n]d[i]>/<d[i]^2>  (first actual, then assumed)\n");
    printf("'splksi' : ksi, dev_from_assumed (first min, then max, then |min|+|max|\n");
    if (fullon) {
	printf("'splstat': l, deg.free, rms [, P(rms)], dws, Plow(dws), Pup(dws)\n");
	if (!logtrans)
	    printf("'splres' : x, y, S(x), d, s^-2, S'(x), S''(x)\n");
	else
	    printf("'splres' : log(x), y, S(x), d, s^-2, S'(x), S''(x)\n");
    }
    printf("'splcall': Three lines of numerical data, for possible use elsewhere\n");
    printf("\n");
    printf("=====================================================================\n");
    printf("To use the spline S(x) after fitting, run:\n");
    printf("   evalsp splrep [evalsp_options...]\n");
    printf("or, together with the fitting in one pass:\n");
    printf("   spline2 [datafile] [options] -q | evalsp [evalsp_options]\n");
    printf("\n");
    printf("     Always output -> file 'evlres': x, S(x), S'(x), S''(x)\n");
    printf("                      By default: in 2001 equidist. x-values\n");
    printf("evalsp_options:\n");
    printf("[-x xbeg xend xstep]         ...: in these equidist. x-values\n");
    printf("[-f xfilename                ...: in x-values listed in xfile\n");
    printf("[-F sbeg send sstep]  Fourier transform of S(x)\n");
    printf("                      (output -> file 'evlfour': s, Re(FT), Im(FT))\n");
    printf("\n");
    printf("                      Output -> screen, and -> file 'evlsum':\n");
    printf("                      ---------------------------------------\n");
    printf("[-e]                  Show maxima and minima of S(x)\n");
    printf("[-i]                  Show inflection points of S(x)\n");
    printf("[-v val]              Show x-value at which S(x)=value\n");
    printf("[-a]                  Show integral of S(x)\n");
    printf("[-h [-b val]]         Show points at half-height of peak\n");
    printf("                      (-b forces the background at val)\n");
    printf("\n");
    printf("Output files of evalsp: 'evlres', 'evlsum' [,'evlfour'], see above (->)\n");
    printf("=====================================================================\n");
    exit(1);
}


/*---------------------------------------------------------------------------*/
/*				   Main 				     */
/*---------------------------------------------------------------------------*/
int main(int argc, char *argv[])
{
    register int i, j;
    int    lineno = 1;
    int    xflag = 0;
    int    Xflag = 0;
    int    nn = 0;
    int    m;
    int    ln;
    int    lind = 0;
    int    lopt = 0;
    int    lmax;
    int	   ldone = 0; /* v5.0 */
    double a;
    double xbegin = 0.0;
    double xend   = 0.0;
    double Xbegin = 0.0;
    double Xend   = 0.0;
    double rmmin  = 0.0;
    double dummy;
    char   dum;
    char*  infile = "STDIN";
    char*  outfile = "STDOUT";
    char   Accept;	    /* indicator if final result is satisfying */
    char   line[200];	    /* line read from input */
    char   rmsversion[80];  /* scaling mode of "rms" variable */

    alarm(6);
    /* ---INITIALIZATION---*/
    fp_in  = stdin;
    fp_out = stdout;
    quite = 0;
    if (isatty(0))
	interactive = 1;
    else 
	interactive = 0;

    fix = rel = done = 0;
    allownonopt = 1;
    sigma = njan = 1;
    k = 4;
    rejlev = 0.05;
    ksi = 0.0;
    acfindex = 1;
    acfsearch = 0;
    nset = ksiset = 0;
    ksibegin = ksiend = 0.0;
    nmin = nmax = 1;
    fullon = 0;    /* reverted to default fullon = 0 */

    /* ---COMMAND LINE OPTIONS--- */
    if (argc == 1)
	usage();

    /* ---GET RUN TIME OPTIONS---*/
    if (argc > 1)
    {
	for (m=1; m<argc; m++)
	{
	    switch( *argv[m] )
	    {
	    case '-':
		switch( *(argv[m]+1) ) {
		case 's' : 
		    acfsearch = 1;
		    break;
		case 'i' :
		    acfindex = atoi(argv[++m]);
		    break;
		case 'X' :
		    Xbegin = atof(argv[++m]);
		    Xend = atof(argv[++m]);
		    Xflag = 1;
		    break;
		case 'x' : 
		    xbegin = atof(argv[++m]);
		    xend = atof(argv[++m]);
		    xflag = 1;
		    break;
		case 'l' : 
		    logtrans = 1;
		    break;
		case 'n' :
		    nmin = nmax = atoi(argv[++m]);
		    nset = 1;
		    break;
		case 'K' :
		    ksibegin = ksiend = atof(argv[++m]);
		    ksistep = 1.0;
		    ksiset = 1;
		    break;
		case 'L':
		    rejlev = atof(argv[++m]);
		    break;
		case 'k':
		    k = atoi(argv[++m]);
		    if (k > KMAX) {
			fprintf(stderr," Spline order set to %d (max)\n",
				KMAX);
			k = KMAX;
		    }
		    if (k < 1) {
			fprintf(stderr," Spline order set to 1 (min)\n");
			k = 1;
		    }
		    break;
		case 'r':
		    rel = 1;
		case 'f':
		case 'a': 
		    fix = 1;
		    fixval = atof(argv[++m]);
		    break;
		case 'O':
		    lopt = atoi(argv[++m]);
		    lind = 1;
		    break;
		case 'o':
		    outfile = argv[++m];
		    if ((fp_out = fopen(outfile, "w")) == NULL) {
			fprintf(stderr, "can not open file %s (%s)\n",
				outfile, strerror(errno));
			exit(1);
		    }
		    break;
		case 'e':
		    allownonopt = 0;
		    break;
		case 'q':
		    quite = 1;
		    interactive = 0;
		    break;
		case 'F':
		    fullon = 1;
		    break;
		default : 
		    usage();
		    break;
		}
		break;
	    default : 
		infile = argv[m];
		if ((fp_in = fopen(infile,"r")) == NULL) {
		    printf("Cannot open %s\n", infile);
		    exit(1);
		}
	    }
	}
    }

    /* ---OPEN FILES FOR WRITING---*/
    if (fullon) {
        if ((fpr = fopen("splstat","w")) == NULL) {
	    fprintf(stderr, "can not open file [splstat] for write (%s)\n",
		    strerror(errno));
	    exit(1);
	}
	if ((fpp = fopen("splres","w")) == NULL) {
	    fprintf(stderr, "can not open file [splres] for write (%s)\n",
		    strerror(errno));
	    exit(1);
	}
	if ((fpc = fopen("splcall","w")) == NULL) {
	    fprintf(stderr, "can not open file [splcall] for write (%s)\n",
		    strerror(errno));
	    exit(1);
	}
	if ((fpa = fopen("splacf","w")) == NULL) {
	    fprintf(stderr, "can not open file [splacf] for write (%s)\n",
		    strerror(errno));
	    exit(1);
	}
	if ((fpk = fopen("splksi","w")) == NULL) {
	    fprintf(stderr, "can not open file [splksi] for write (%s)\n",
		    strerror(errno));
	    exit(1);
	}
	if ((fpu = fopen("splsum","w")) == NULL) {
	    fprintf(stderr, "can not open file [splsum] for write (%s)\n",
		    strerror(errno));
	    exit(1);
	}
    }

    /* ---READ DATA--- */
    maxweight = -99.99;
    i = 1;
    while (fgets(line,sizeof(line),fp_in)!=NULL) {
	j = sscanf(line,"%lf %lf %lf %lf", tau+i, gtau+i, weight+i, &a);
	if (j<2) {
	    fprintf(stderr,
		    "Line %d has fewer than two numbers: skipped\n", lineno);
	    lineno++;
	    continue;
	}
	lineno++;
	if (i==1) {
	    /* determine # input columns */
	    if (j==3) sigma = 0;
	    else if (j==4 || j<2)
	    {fprintf(stderr,"Wrong data format\n"); exit(1);}
	}
	if (i>NDATMAX)
	{fprintf(stderr,"Line %d: too many data. Increase NDATMAX\n", i);
	    exit(1);}
	if (sigma==0) {
	    /* sigmas are supposed to be in file */
	    if (j==2) weight[i] = weight[i-1];
	    if (j==3) {
		if (weight[i]<=0.0)
		{fprintf(stderr,"Line %d: zero or negative s\n", i);
		    exit(1);}
		weight[i] = 1.0/weight[i]/weight[i];
	    }
	}
	if (sigma==1) {
	    /* sigmas are not supposed to be in file */
	    weight[i] = 1.0;
	}
	if (fix==1) {
	    /* sigmas freezed: override assignments */
	    if (rel==0) {
		/* absolute sigma freezed */
		if (fixval>0.0) 
		    weight[i] = 1.0/fixval/fixval;
		/* if (fixval==0.0) weight was already assigned */
		if (fixval<0.0) {
		    if (gtau[i]==0.0)
			/* temporary value: */
			weight[i]= -999.999;
		    else {
			nonsig = log10(fabs(gtau[i]));
			/* repaired 26-2-1996 */
			/*nonsig = floor(nonsig)+fixval;*/
			nonsig = floor(nonsig)+fixval+1.0;
			nonsig = exp(2.302585*nonsig);
			nonsig /= 3.464102;
			weight[i] = 1.0/nonsig/nonsig;
			if (weight[i]>maxweight)
			    maxweight=weight[i];
		    }
		}
	    }
	    if (rel==1) {
		/* relative sigma freezed */
		if (fixval>0.0) {
		    weight[i] = fixval*fabs(gtau[i]);
		    weight[i] = 1.0/weight[i]/weight[i];
		}
		if (fixval<0.0) {
		    weight[i] = -fixval*sqrt(fabs(gtau[i]));
		    weight[i] = 1.0/weight[i]/weight[i];
		}
	    }
	}
	/* weight assigned; accept point for fit? */
	a = tau[i];
	if ((xflag==0 && Xflag==0) ||
	    (xflag==1 && Xflag==0 && a>=xbegin && a<=xend) ||
	    (xflag==0 && Xflag==1 && (a>Xend || a<Xbegin)) ||
	    (xflag==1 && Xflag==1 &&
	     ((a>=xbegin && a<Xbegin) || (a>Xend && a<=xend))))
	    i++;
    } /* end of read loop */

    ntau = i-1;

    /* If data have certain number of significant digits, assign
       largest weight to the data value(s) zero */
    if (fix==1 && rel==0 && fixval<0.0) {
	for (i=1; i<=ntau; i++) {
	    if (weight[i]== -999.999) weight[i]=maxweight;
	}
    }

    if (lopt >= .8*ntau || lind == 0) lopt = .8*ntau-1;
    km1 = k-1;
    lmax = ntau-km1-1;
    if (lmax < lopt) lopt = lmax;
    for (i = 1; i <= ntau; i++) totalw += weight[i];
    taumax = taumin = gtau[1];
    for (i = 2; i <= ntau; i++)
    {
	if (gtau[i] < taumin) taumin = gtau[i];
	if (gtau[i] > taumax) taumax = gtau[i];
    }
    if (logtrans != 0) for(i = 1; i <= ntau; i++) {
	    if (tau[i]>0.0) tau[i] = log10(tau[i]);
	    else {
		fprintf(stderr,"X-datapoint %d: cannot take log of %g\n",
			i, tau[i]);
		exit(1);
	    }
	}

    /* reading done */

    /* ---CLOSE READFILE--- */
    if (fp_in != stdin) fclose(fp_in);

    /* ---COMPUTE AVERAGE DATA SPACING--- */	
    dxav = (tau[ntau]-tau[1])/(double)(ntau-1);

    /* ---MESSAGES--- */
    if (!quite) {
	fprintf(stderr,"............. spline2 version %s ...............\n", 
		VERSION);
	fprintf(stderr,"Name of data file: %s\n", infile);
	if (sigma==1)
	    fprintf(stderr,"Format: x y\n");
	if (sigma==0)
	    fprintf(stderr,"Format: x y s\n");
	if (logtrans==1)
	    fprintf(stderr,"x -> log10(x)\n");
	if (xflag==1)
	    fprintf(stderr,"X-range limited to (%g, %g)\n", xbegin, xend);
	if (Xflag==1)
	    fprintf(stderr,"X-range (%g, %g) cut out\n", Xbegin, Xend);
	fprintf(stderr,"Number of datapoints participating in spline-fit = %d\n",
		ntau);
	if (lind == 1)
	    fprintf(stderr,"Search-reversal point set at l=%d\n", lopt);
	fprintf(stderr,"Degree of spline = %d\n", k-1);

	if (fix==1) {
	    fprintf(stderr,"Uncertainties in y frozen at ");
	    if (rel==0) {
		if (fixval==0.0) fprintf(stderr,"values specified in input\n");
		if (fixval>0.0) fprintf(stderr,"%g\n", fixval);
		if (fixval<0.0) fprintf(stderr,"%g-th digit\n", -fixval+1);
	    }
	    if (rel==1) {
		if (fixval>0.0) fprintf(stderr,"%g*|y|\n", fixval);
		if (fixval<0.0) fprintf(stderr,"%g*sqrt(|y|)\n", -fixval);
	    }
	}
	fprintf(stderr,"Spline approximations are tested according to the ");
	if (fix==0)
	    fprintf(stderr,"Durbin-Watson test\n");
	else
	    fprintf(stderr,"Chi-square test\n");
	if (acfsearch)
	    fprintf(stderr,"Automatic search for autocorrelation in residuals\n");
	if (ksiset)
	    fprintf(stderr,"Assumed autocorrelation length ksi = %g (fixed)\n", ksibegin);
	if (nset) {
	    fprintf(stderr,"Autocorrelation tested only for datapoint index spacing = %d\n", 
		    nmin);
	    if(!ksiset) 
		fprintf(stderr,"Assumed autocorrelation length ksi = %g (fixed)\n", ksibegin);
	}
	if (!nset && !ksiset && !acfsearch) {
	    fprintf(stderr,"Autocorrelation tested only for datapoint index spacing = %d\n", 
		    nmin);
	    fprintf(stderr,"Assumed autocorrelation length ksi = %g (fixed)\n", ksibegin);
	}
	fprintf(stderr,"Assumed autocorrelation function of residuals: ");
	switch(acfindex) {
	case 1:
	    fprintf(stderr,"exponential: exp(-x/ksi)\n");
	    break;
	case 2:
	    fprintf(stderr,"gaussian: exp(-x^2/(2ksi^2))\n");
	    break;
	case 3:
	    fprintf(stderr,"linear: 1-x/(2ksi)\n");
	    break;
	case 4:
	    fprintf(stderr,"sinc: sin(2x/ksi)/(2x/ksi)\n");
	    break;
	}
	fprintf(stderr,"Average datapoint spacing <delta x> = %g\n", dxav);
	if (allownonopt == 1)
	    fprintf(stderr,"Splines with non-optimized breakpoints are allowed\n");
	else
	    fprintf(stderr,"Only splines with optimized breakpoints are allowed\n");
    }
		
    /* ---WRITE FILE splcall TO SHOW OTHER PROGRAMS WHAT HAS BEEN DONE--- */
    if (fpc) {
	fprintf(fpc,"%d %d %d %g %g %d %g %g\n",
		sigma, logtrans, xflag, xbegin, xend, Xflag, Xbegin, Xend);
	fprintf(fpc,"%d %d %d %d %d %d %d %g\n",
		ntau, fullon, lopt, allownonopt, k-1, fix, rel, fixval);
	fprintf(fpc,"%d %g %d\n",
		( acfsearch || (ksiset && !nset) ) ? -1 : nmin,
		acfsearch ? -1 : ksibegin,
		acfindex);
    }
	
    /* ---DETERMINE AUTOMATIC KSI-RANGE--- */
    /* v6.0 */
    if (acfsearch) {
	ksibegin = 0.0;
	ksistep = dxav/5.0;
	ksiend = 3.0*dxav;
	/* v6.1 */
	iksi = 0;
	/* array overflow will be tested on the fly */
    }

    /* ---MAIN ALGORITHM--- */

    ksi = ksibegin;
    if (!interactive && !quite) fprintf(stderr,"ksi-sweep: ");

	
    /* ----------------------------------------------------- */
    /* begin step 1: equal-information splines, increasing l */
    /* ----------------------------------------------------- */

    /* v6.0: return point for autocorrelation search */
lbegin2:
	
    if (!interactive && !quite)
	fprintf(stderr,"+");
    il2sub = 0;

    /* ---COMPUTE MAXIMUM N FOR ACF TESTING--- */
    /* v6.0 Combined check on correlation-corrected dws for n=1..nmax */
    if (acfsearch || (ksiset && !nset) ) {
	nmin = 1;
	nmax = (int)(ACFRANGE1*ksi/dxav+0.5)+ACFRANGE2;
    }
		
    if (nmax > ACFMAX) {
	nmax = ACFMAX;
	fprintf(stderr, "Preventing ACF size overflow by limiting nmax to %d\n",
		nmax);
    }

    lnew = 1;

    /* v5.0: return point for hyperspline extension */
lbegin1:

    test = 0.0;
    while (test < rejlev && lnew <= lopt && lnew <= LMAX)
    {
	l2sub(&lnew,0,1);
	ln = dummy = UPFACTOR*lnew;
	ln = TRUNC_SPEC(ln,dummy);
	if (ln > lopt && lnew < lopt) ln = lopt;
	if (ln == lnew) lnew += 1; else lnew = ln;
	if (lind == 1) test = 0.0;
	else test = (fix == 0) ? ltest : ctest;
    }
    nl2sub = il2sub;
    lbeg = iresul[nl2sub][1];

    /* -------------------------------------------------- */
    /* begin step 2: knot-optimized splines, decreasing l */
    /* -------------------------------------------------- */

    lnew = dummy = DOWNFACTOR*lbeg;
    lnew = TRUNC_SPEC(lnew,dummy);
    while (lnew > 1 && lnew <= LMAX) {
	l2sub(&lnew,1,1);
	lnew = dummy = DOWNFACTOR*lnew;
	lnew = TRUNC_SPEC(lnew,dummy);
    }
    if (lbeg > 1) {
	/* not essential: just for generating the final lines
	   in the "search-path-plot" */
	il2sub++;
	for (j = 1; j <= 2; j++) iresul[il2sub][j] = iresul[1][j];
	for (j = 1; j <= 7; j++) resul[il2sub][j] = resul[1][j];
    }

    /* ---------- */
    /* end step 2 */
    /* ---------- */

    Accept = 'n';
    while (Accept == 'n') {
	/* ---------------------------------------------------------- */
	/* step 4: search for best spline in this decreasing-l series */
	/* ---------------------------------------------------------- */

	/* Print some messages */
	if (interactive && !done)
	{
	    printf("'rms' means: (Fit-estimated y-uncertainty)");
	    if (fix==0)
	    {
		if (sigma==1) sprintf(rmsversion,".");
		if (sigma==0)
		    sprintf(rmsversion," / (uncertainty given in input file).");
	    }
	    if (fix==1)
	    {
		if (rel==0 && fixval>0.0)
		    sprintf(rmsversion," / %g.", fixval);
		else
		    if (rel==0 && fixval==0.0)
			sprintf(rmsversion," / (uncertainty given in input file.)");
		    else
			sprintf(rmsversion," / (uncertainty given on command line.)");
	    }
	    printf("%s\n", rmsversion);
	    printf("...........................................................\n");
	    printf("Suggestion(s) for a good spline-fit:\n");
	}

	/* Determine best fit for the current decreasing-l optimization
	   series */
	wind = windbp = nn = 0;
	if (fix == 0)
	    /* If errors are not fixed: dws test */
	{
	    /* check results so far, first from the conservative
	       viewpoint (j=5), and if no acceptable spline
	       can be found, also from the liberal viewpoint
	       (j=4).
	    */
	    for (j = 5; j >= 4 && nn == 0; j--)
	    {
		if (j == 4) wind = 1;
		/* only the most recent decreasing-l series */
		for (i = nl2sub; i < il2sub; i++)
		{
		    /* suggest spline i when
		       no singularities were found AND
		       spline i is acceptable AND
		       spline i+1 is not acceptable
		    */
		    if (resul[i][1] > 0.0 &&
			resul[i][j] > rejlev &&
			resul[i+1][j] < rejlev)
		    {
			if (interactive && !done)
			    printf("rms = %g, dws = %g: l=%d\n",
				   resul[i][1], resul[i][3],
				   iresul[i][1]);
			if (windbp==1) windbp = 0;
			if (iresul[i][1]==lbeg) windbp = 1;
			isug = i;
			nn++;
		    }
		}
		if (nl2sub == 1 && resul[i][j] > rejlev)
		{
		    if (interactive && !done)
			printf("rms = %g, dws = %g: l=1\n",
			       resul[il2sub][1], resul[il2sub][3]);
		    if (iresul[i][1]==lbeg) windbp = 1;
		    isug = il2sub;
		    nn++;
		}
	    }
	}
	else    /* If errors are fixed: chi-squared test */
	{
	    for (i = nl2sub; i < il2sub; i++)
	    {
		if (resul[i][1] > 0.0 &&
		    resul[i][2] > rejlev &&
		    resul[i+1][2] < rejlev)
		{
		    if (interactive && !done)
			printf("rms = %g, dws = %g: l=%d\n",
			       resul[i][1], resul[i][3], iresul[i][1]);
		    if (windbp==1) windbp = 0;
		    if (iresul[i][1]==lbeg) windbp = 1;
		    isug = i;
		    nn++;
		}
	    }
	    if (nl2sub == 1)
	    {
		if (interactive && !done)
		    printf("rms = %g, dws = %g: l=1\n",
			   resul[il2sub][1], resul[il2sub][3]);
		if (iresul[i][1]==lbeg) windbp = 1;
		isug = il2sub;
		nn++;
	    }
	}

	/* What if no acceptable fit can be found? */
	if (nn == 0)
	{
	    if (interactive && !done)
	    {
		printf("No really good spline can be found...");
		printf(" You'll have to live with this one:\n");
	    }
	    rmmin = resul[nl2sub][1];
	    j = nl2sub;
	    for (i = nl2sub; i <= il2sub; i++)
	    {
		if (resul[i][1] < rmmin)
		{
		    rmmin = resul[i][1];
		    j = i;
		}
	    }
	    if (interactive && !done)
		printf("rms = %g, dws = %g: l=%d\n", resul[j][1],
		       resul[j][3], iresul[j][1]);
	    if (windbp==1) windbp = 0;
	    if (iresul[i][1]==lbeg) windbp = 1;
	    isug = j;
	}

	/* v5.0: "Hyperspline" extension starts here */

	/* Check warning status. Does lrev have to be increased? */
	if ( (wind == 1 || (!allownonopt && windbp == 1) ) && !done)
	{
	    if (interactive && wind == 1)
		printf("Mild warning: all suggested fits are in the indecisive D-W area\n");
	    if (interactive && windbp == 1)
		printf("Mild warning: suggested fit has non-optimized breakpoints \n");
	    if (lind != 1)
	    {
		lopt = dummy = UPFACTOR*lbeg;
		lopt = TRUNC_SPEC(lopt,dummy);
		if (lopt==lbeg) lopt++;
		if (lopt > MAXHYPFRAC*ntau) {
		    if (interactive)
			fprintf(stderr,"End of lrev increments:  will not increase lrev to %d\n", lopt);
		}
		else {
		    lnew = lopt;
		    if (interactive)
			printf("Now increasing lrev from %d to %d\n", lbeg, lopt);
		    goto lbegin1;
		}
	    }
	}

	/* Here the best possible spline has been identified for the
	   current value of ksi. */

	/* Now store parameters necessary for final re-calculation:
	   for spline:
	   lbeg      = starting value of l
	   lfin      = final value of l
	   for Q (dw-statistic):
	   nmin      = lowest data index spacing for acf
	   nmax      = highest data index spacing for acf
	   ksi       = autocorrelation distance
	   acfdevmin = minimum deviation from assumed acf
	   acfdevmax = maximum deviation from assumed acf
	   for diagnostics message:
	   nn        = number of good fits found
	   wind      = indicator for indecisive fit
	*/

	iksi++;
	iresulksi[iksi][1] = lbeg;
	iresulksi[iksi][2] = iresul[isug][1];  /* lfin */
	iresulksi[iksi][3] = nn;
	iresulksi[iksi][4] = wind;
	iresulksi[iksi][5] = nmin;
	iresulksi[iksi][6] = nmax;
	resulksi[iksi][1]  = ksi;
	resulksi[iksi][2]  = resul[isug][6];	/* acfdevmin */
	resulksi[iksi][3]  = resul[isug][7];	/* acfdevmax */
	resulksi[iksi][4]  = resul[isug][1];	/* rms */
	resulksi[iksi][5]  = resul[isug][3];	/* dws */

	/* Next ksi */
	if (iksi > KSIMAX) {
	    fprintf(stderr,
		    "Warning: ksi testrange abnormally ended because of array overflow\n");
	    fprintf(stderr,"Last ksi = %g\n", ksi);
	}
	else {
	    if (ksi < ksiend) {
/* V6.0a Try out a finer step for ksi < dxav */
		ksi += (ksi < dxav ? ksistep/2.0 : ksistep);
		if (interactive)
		    printf("Now increasing ksi to %g\n", ksi);
		goto lbegin2;
	    }
	}
	nksi = iksi;

	if (!interactive && !quite) fprintf(stderr," end sweep\n");

	/* All ksi's have been tested. Now find the one that
	   yielded a spline fit with the residual acf that agrees
	   best with the assumed acf */
		   

	acffit = fabs(resulksi[1][2])+fabs(resulksi[1][3]);
	iksisug = 1;
	for(iksi=1; iksi<=nksi; iksi++) {
	    dummy = fabs(resulksi[iksi][2])+fabs(resulksi[iksi][3]);
	    if (dummy < acffit) {
		iksisug = iksi;
		acffit = dummy;
	    }
	}

	if (fpk) {
	    for(iksi=1; iksi<=nksi; iksi++) {
		fprintf(fpk, "%g %g\n", resulksi[iksi][1], resulksi[iksi][2]);
	    }
	    for(iksi=1; iksi<=nksi; iksi++) {
		fprintf(fpk, "%g %g\n", resulksi[iksi][1], resulksi[iksi][3]);
	    }
	    for(iksi=1; iksi<=nksi; iksi++) {
		dummy = fabs(resulksi[iksi][2])+fabs(resulksi[iksi][3]);
		fprintf(fpk, "%g %g\n", resulksi[iksi][1], dummy);
	    }
	}


	/* The best ksi and the corresponding best spline are known,
	   so that the final outcome can be presented */

	lbeg = iresulksi[iksisug][1];
	lfin = iresulksi[iksisug][2];
	nn = iresulksi[iksisug][3];
	wind = iresulksi[iksisug][4];
	nmin = iresulksi[iksisug][5];
	nmax = iresulksi[iksisug][6];
	ksi = resulksi[iksisug][1];
	acfdevmin = resulksi[iksisug][2];
	acfdevmax = resulksi[iksisug][3];
	rms = resulksi[iksisug][4];	
	dws = resulksi[iksisug][5];
/* Debug */	
	if (interactive) {
	    iksi = iksisug;	
	    fprintf(stdout,
		    "<> ksi= %g small= %g big= %g rms= %g Q= %g ",
		    resulksi[iksi][1],
		    resulksi[iksi][2],
		    resulksi[iksi][3],
		    resulksi[iksi][4],
		    resulksi[iksi][5]);
	    fprintf(stdout,
		    "lbeg= %d lfin= %d nn= %d wind= %d nmin= %d nmax= %d\n",
		    iresulksi[iksi][1],
		    iresulksi[iksi][2],
		    iresulksi[iksi][3],
		    iresulksi[iksi][4],
		    iresulksi[iksi][5],
		    iresulksi[iksi][6]);
	};


	/* In interactive mode: allow user a possibly different choice
	   for l (this has become somewhat minimal, because ksi can
	   not be hand-picked anymore; add this option? */
	if (interactive) {
	    if (!done)
		printf("Non-optimized breakpoints will be used for l=%d and above\n", lbeg);
	    printf("Choose the number of intervals for the spline \
('s' for suggested)? l=");
	    if (scanf("%d", &lfin)==0) lfin=iresulksi[iksisug][2];
	}
	else { /* non-interactive */
	    if (!quite) {
		fprintf(stderr,"Spline-fit: rms=%g, dws=%g, l=%d (%s) ksi=%g acffit=%g ",
			rms, dws, lfin, (lfin>=lbeg) ? "eqi" : "opt", ksi, acffit);
		if (nn == 0) 
		    fprintf(stderr,"(no good fit) ");
		if (wind == 1) 
		    fprintf(stderr,"(DW indecisive) ");
		fprintf(stderr,"\n");
	    }

	    if (fpu) {
		fprintf(fpu,"rms    %g\n", rms);
		fprintf(fpu,"dws    %g ", dws);
		if (nn == 0) fprintf(fpu,"(no good fit) ");
		if (wind == 1) fprintf(fpu,"(DW indecisive) ");
		fprintf(fpu,"\n");
		fprintf(fpu,"l      %d (%s)\n", lfin,
			(lfin>=lbeg) ? "eqi" : "opt");
		fprintf(fpu,"ksi    %g\n", ksi);
		fprintf(fpu,"acffit %g\n", acffit);
	    }
	}

	/* ---------------------------------------------- */
	/* begin step 5: re-calculate the selected spline */
	/* ---------------------------------------------- */


	if (lfin > lbeg && lfin <= LMAX)
	    l2sub(&lfin,0,0);
	else
	{
	    if (lbeg <= LMAX) {
		l2sub(&lbeg,0,0);
		lnew = dummy = DOWNFACTOR*lbeg;
		lnew = TRUNC_SPEC(lnew,dummy);
		while (lnew >= lfin && lnew <= LMAX)
		{
		    l2sub(&lnew,1,0);
		    lnew = dummy = DOWNFACTOR*lnew;
		    lnew = TRUNC_SPEC(lnew,dummy);
		}
		if (l > lfin && lfin <= LMAX) l2sub(&lfin,1,0);
	    }
	}

	if (interactive) {
	    printf("rms = %g", rms);
	    printf(", dws = %g.", dws);
	    if (fix == 0) test = ltest;
	    else test = ctest;
	    if (test < rejlev)
		printf(" <-- WARNING: not ok. ");
	    else {
		if (fix == 0 && utest < rejlev) {
		    printf(" <-- Mild warning: indecisive. ");
		}
	    }
	    printf(" Done (y/n)? ");
	    scanf("%c%c", &dum, &Accept);
	    scanf("%c", &dum);
	    done = 1;
	}
	else
	    Accept = 'y';
    }

    /* Still needs to write to splsum if mode is interactive (added 20/2/02) */
    if (interactive) {
	if (fpu) {
	    fprintf(fpu,"rms    %g\n", rms);
	    fprintf(fpu,"dws    %g ", dws);
	    if (nn == 0) fprintf(fpu,"(no good fit) ");
	    if (wind == 1) fprintf(fpu,"(DW indecisive) ");
	    fprintf(fpu,"\n");
	    fprintf(fpu,"l      %d (%s)\n", lfin,
		    (lfin>=lbeg) ? "eqi" : "opt");
	    fprintf(fpu,"ksi    %g\n", ksi);
	    fprintf(fpu,"acffit %g\n", acffit);
	}
    }

    /*---WRITE TO OUTPUT FILE(S)---*/
    l2err(1,0);

    fprintf(fp_out, "%d %d\n", k, l);
    for (i = 1; i <= l+1; i++) fprintf(fp_out, "%.8g\n", brek[i]);
    for (i = 1; i <= l+km1; i++) fprintf(fp_out, "%.8g\n", bcoef[i]);

    /* v5.0 try to write spline statistics to file in a more or
       less organized way, so that plotting becomes simple.
       (suspected bug: maybe this does not work so nicely when the
       user searches 'manually')
	   
       V6: This is almost useless now when autosearch is on, since
       only the results for the last ksi-trial are available
    */

    if (fullon)
    {
	/* 1st pass to splstat:
	   only equi-information splines (increasing l) */
	for (i = 1; i <= il2sub; i++)
	{
	    if (iresul[i][1] > ldone)
	    {
		fprintf(fpr, " %d %d %g",
			iresul[i][1], iresul[i][2], resul[i][1]);
		if (fix == 1) fprintf(fpr, " %g", resul[i][2]);
		fprintf(fpr, " %g %g %g\n",
			resul[i][3], resul[i][4], resul[i][5]);
		ldone = iresul[i][1];
		/* mark all but the highest of a series */
		if (i<il2sub && iresul[i+1][1] > iresul[i][1])
		    iresul[i][1] = 0;
	    }
	}
	/* 2nd pass to splstat: all newnot splines (decreasing l) */
	for (i = 1; i <= il2sub; i++)
	{
	    if (iresul[i][1] != 0)
	    {
		fprintf(fpr, " %d %d %g",
			iresul[i][1], iresul[i][2], resul[i][1]);
		if (fix == 1) fprintf(fpr, " %g", resul[i][2]);
		fprintf(fpr, " %g %g %g\n",
			resul[i][3], resul[i][4], resul[i][5]);
	    }
	}
/*
  THIS IS VERY LAZY:
*/
	/* 3rd pass to splstat: final spline */
	fprintf(fpr, " %d -999 %g", lfin, rms);
	if (fix == 1) fprintf(fpr, " -999");
	fprintf(fpr, " %g -999 -999\n", dws);
    }

    /* Write actual and assumed autocorrelation functions (e.g. for plotting) */
    l2err(0,0);
    if (fpa) {
	for (njan=0; njan<=nmax; njan++)
	    fprintf(fpa,"%d %g\n", njan, acfa[njan]);
	for (njan=0; njan<=nmax; njan++)
	    fprintf(fpa,"%d %g\n", njan, acfv[njan]);
    }

    /* ---WRAPPING UP--- */
    if (fp_out != stdout) fclose(fp_out);
    if (fpc) fclose(fpc);
    if (fpa) fclose(fpa);
    if (fpk) fclose(fpk);
    if (fpu) fclose(fpu);

    if (fpr) fclose(fpr); 
    if (fpp) fclose(fpp);

    exit(0);
}

/*---------------------------------------------------------------------------*/
/*				Functions				     */
/*---------------------------------------------------------------------------*/
/*	Calculates spline approximation to the data.
 *
 *	input:
 *	   *iptr   = number of intervals;
 *	    iopt   = 0: intervals will contain equal numbers of
 *		        data points,
 *		   = 1: optimize breakpoint locations with respect
 *			to the previous locations;
 *	    wr     = 0: nothing is written to iresul[] and resul[],
 *	           = 1: results are written to iresul[] and resul[].
 *
 *	output:
 *	   iok = 1 if global Powell test is ok,
 *	       = 0 otherwise,
 *	   ipowt = sum of local Powell numbers,
 *	   rms = weighted rms error for fit, should be equal to
 *	         about 1.0 for well estimated data uncertainty,
 *	   nsing = number of singularities found while solving the
 *	   	   normal equations,
 *	   ipo   = number of intervals where local powell test fails,
 *	   brek = the new breakpoint sequence,
 *	   coef = the matrix of the (i-1)th derivatives (to the right)
 *	          in the jth breakpoint,
 *	   ftau = the spline at the data points,
 *	   q = normalized errors at the data points.
 */
void l2sub(int *iptr, int iopt, int wr)
{
    int	ip;
    double coefg[3][LMAX+1];
    double p[KMAX+1][NMAX+1];
    double scrtch[NMAX+1];
    double t[NTMAX+1];
    double chi;

    ip = *iptr;
    if (iopt == 0) ip = -ip;
    newknt(brek,coef,&l,&k,scrtch,&ip,coefg);
    l2knts(scrtch,&ip,&k,t,&n);
    l2appr(t,&n,&k,p,scrtch,bcoef);
    bsplpp(t,bcoef,&n,&k,p,brek,coef,&l);
    l2err(0,1);
    dwtest(dws,&ltest,&utest);
    if (fix == 1) {
	chi = rms*rms*freed;
	ctest = chitest(chi,freed);
    }
    if (wr == 1) {
	il2sub++;
	iresul[il2sub][1] = ip;
	iresul[il2sub][2] = freed;
	if (il2sub == 1) {
	    if (fix == 0)
		rmsmin = rms;
	    else {
		rmsmin = 1.;
		rmsmax = 5.;
	    }
	}
	if (rms < rmsmin) rmsmin = rms;
	if (rms > rmsmax) rmsmax = rms;
	if (nsing > 0) rms *= -1;
	resul[il2sub][1] = rms;
	if (fix == 1) resul[il2sub][2] = ctest;
	else resul[il2sub][2] = 0;
	resul[il2sub][3] = dws;
	resul[il2sub][4] = ltest;
	resul[il2sub][5] = utest;
	resul[il2sub][6] = acfdevmin;
	resul[il2sub][7] = acfdevmax;
    }
}

/*---------------------------------------------------------------------------*/
/*  calculates new ("best") set of breakpoints, no intervals with
 *  fewer then two data-points will be made
 */
void newknt(double ara[],double dara[][NMAX+1],int* iptr,int* jptr,
	    double arb[],int* kptr, double darb[][LMAX+1])
{
    int	ip, jp, kp, i, ii, j, lleft, mflag, iend, idif;
    double  a, oneovk, dif, difprv, step, stepi;
    ip = *iptr;
    jp = *jptr;
    kp = *kptr;

    arb[1] = tau[1];
    arb[abs(kp)+1] = tau[ntau];
    if(kp < 0) {
	kp = -1*kp;
	/*$dir no_recurrence*/
	for (i = 2; i <= kp; i++) {
	    a = ((double)(i-1))*(ntau-1)/kp+1;
	    ii = a;
	    arb[i] = tau[ii]+(a-ii)*(tau[ii+1]-tau[ii]);
	}
	*kptr = kp;
    }
    else {
	oneovk = 1./jp;
	darb[1][1] = 0.;
	difprv = fabs((dara[jp][2]-dara[jp][1])/(ara[3]-ara[1]));
	for (i = 2; i <= ip; i++) {
	    dif = fabs((dara[jp][i]-dara[jp][i-1])/(ara[i+1]-ara[i-1]));
	    darb[2][i-1] = pow((dif+difprv),oneovk);
	    darb[1][i] = darb[1][i-1]+darb[2][i-1]*(ara[i]-ara[i-1]);
	    difprv = dif;
	}
	darb[2][ip] = pow((2*difprv),oneovk);
	step = (darb[1][ip]+darb[2][ip]*(ara[ip+1]-ara[ip]))/kp;
	if (step > 0.) {
	    j = 1;
	    for (i = 2; i <= kp; i++) {
		stepi = (i-1)*step;
		while (j != ip && stepi > darb[1][j+1]) j++;
		if (darb[2][j] != 0) {
		    arb[i] = ara[j]+(stepi-darb[1][j])/darb[2][j];
		}
		else 
		    arb[i] = (ara[j]+ara[j+1])/2;
	    }
	}
	else {
	    step = (tau[ntau]-tau[1])/kp;
	    /*$dir no_recurrence*/
	    for (i = 2; i <= kp; i++) {
		arb[i] = tau[i]+(i-1)*step;
	    }
	}
	lleft = 1;
	iend = idif = 0;
	for (i = 2; i <= kp+1; i++) {
	    interv(tau,&ntau,&arb[i],&left,&mflag);
	    if (left-lleft > 0) 
		lleft = left;
	    else {
		lleft += 1;
		if (lleft < ntau) {
		    arb[i] = (tau[lleft]+tau[lleft+1])/2.;
		}
		else {
		    iend++;
		    arb[i] = tau[ntau];
		}
	    }
	}
	if (iend != 0) {
	    i = 1;
	    lleft = ntau-1;
	    while (idif < 1) {
		interv(tau,&ntau,&arb[kp+1-i],&left,&mflag);
		idif = lleft-left;
		if (idif < 1) {
		    lleft -= 1;
		    arb[kp+1-i] = (tau[lleft]+tau[lleft+1])/2.;
		    i++;
		}
	    }
	}
    }
}

/*---------------------------------------------------------------------------*/
/*  breakpoints to knots  */
void l2knts(double* ara,int* iptr,int* jptr,double* arb,int* kptr)
{
    int	ip, jp, kp, i, kk;
    kk = km1;
    /*$dir no_recurrence*/
    for (i = 1; i <= kk; i++)
	arb[i] = ara[1];
    ip = *iptr;
    jp = *jptr;
    /*$dir no_recurrence*/
    for (i = 1; i <= ip; i++)
	arb[km1+i] = ara[i];
    kp = km1+ip;
    /*$dir no_recurrence*/
    for (i = 1; i <= jp; i++)
	arb[kp+i] = ara[ip+1];
    *kptr = kp;
}

/*---------------------------------------------------------------------------*/
/*  calculates spline approximation to given data (tau,gtau)  */
void l2appr(double ara[],int* iptr,int* jptr,double dara[][NMAX+1],
	    double arb[],double arc[])
{
    int	ip, jp, j, i, ll, mm, jj;
    double  dw, biatx[KMAX+1];
    ip = *iptr;
    jp = *jptr;
    ll = jp;
    for (j = 1; j <= ip; j++) {
	arc[j] = 0.;
	for (i = 1; i <= ll; i++) dara[i][j] = 0.;
    }
    left = jp;
    for (ll = 1; ll <= ntau; ll++) {
	while (left != ip && tau[ll] >= ara[left+1]) 
	    left++;
	bsplvb(ara,jp,1,&tau[ll],&left,biatx);
	for(mm = 1; mm <= jp; mm++) {
	    dw = biatx[mm]*weight[ll];
	    j = left-jp+mm;
	    arc[j] += dw*gtau[ll];
	    i = 1;
	    for (jj = mm; jj <= jp; jj++) {
		dara[i][j] += biatx[jj]*dw;
		i++;
	    }
	}
    }
    bchfac(dara,&jp,&ip,arb);
    bchslv(dara,&jp,&ip,arc);
}

/*---------------------------------------------------------------------------*/
/*  constructs cholesky factorization  */
void bchfac(double dara[][NMAX+1],int* iptr,int* jptr,double  ara[])
{
    int	ip, jp, i, j, imax, jmax, ii;
    double  ratio;
    ip = *iptr;
    jp = *jptr;
    nsing = 0;
    if (jp <= 1 && dara[1][1] > 0) 
	dara[1][1] = 1/dara[1][1];
    else
    {
	/*$dir no_recurrence*/
	for (ii = 1; ii <= jp; ii++)
	    ara[ii] = dara[1][ii];
	for (ii = 1; ii <= jp; ii++) {
	    if (dara[1][ii]+ara[ii] <= ara[ii]) {
		nsing++;
		for(j = 1; j <= ip; j++) dara[j][ii] = 0.;
	    }
	    else {
		dara[1][ii] = 1/dara[1][ii];
		if (ip-1 < jp-ii) imax = ip-1;
		else imax = jp-ii;
		if (imax > 0) {
		    jmax = imax;
		    for(i = 1; i <= imax; i++) {
			ratio = dara[i+1][ii]*dara[1][ii];
			/*$dir no_recurrence*/
			for(j = 1; j <= jmax; j++) {
			    dara[j][ii+i] -= dara[j+i][ii]*ratio;
			}
			jmax--;
			dara[i+1][ii] = ratio;
		    }
		}
	    }
	}
    }
}

/*---------------------------------------------------------------------------*/
/* solves a banded positive definite set of equations */
void bchslv(double dara[][NMAX+1],int* iptr,int* jptr,double  ara[])
{
    int	ip, jp, kk, jmax, j, ii;
    double  nbndm1;
    ip = *iptr;
    jp = *jptr;
    if (jp <= 1)
	ara[1] *= dara[1][1];
    else {
	nbndm1 = ip-1;
	kk = jp;
	for(ii = 1; ii <= kk; ii++) {
	    if (nbndm1 < jp-ii) jmax = nbndm1;
	    else jmax = jp-ii;
	    /*$dir no_recurrence*/
	    if (jmax > 0) 
		for (j = 1; j <= jmax; j++) {
		    ara[j+ii] -= dara[j+1][ii]*ara[ii];
		}
	}
	for (ii = jp; ii >= 1; ii--) {
	    ara[ii] *= dara[1][ii];
	    if (nbndm1 < jp-ii) jmax = nbndm1;
	    else jmax = jp-ii;
	    if (jmax > 0) 
		for (j = 1; j <= jmax; j++) {
		    ara[ii] -= dara[j+1][ii]*ara[j+ii];
		}
	}
    }
}

/*---------------------------------------------------------------------------*/
/*  converts spline to piecewise polynomial representation  */
void bsplpp(double ara[],double arb[],int* iptr,int* jptr,
	    double dara[][NMAX+1],double arc[], double darb[][NMAX+1],int* kptr)
{
    int	ip, jp, lsofar, j, i, jp1, kmj;
    double  diff, sum, biatx[KMAX+1];
    ip = *iptr;
    jp = *jptr;
    arc[1] = ara[jp];
    lsofar = 0;
    for (left = jp; left <= ip; left++) {
	if(ara[left+1] != ara[left]) {
	    lsofar++;
	    arc[lsofar+1] = ara[left+1];
	    if (jp <= 1)
		darb[1][lsofar] = arb[left];
	    else {
		/*$dir no_recurrence*/
		for (i = 1; i <= jp; i++)
		{
		    dara[i][1] = arb[left-jp+i];
		}
		for (jp1 = 2; jp1 <= jp; jp1++)
		{
		    j = jp1-1;
		    kmj = k-j;
		    /*$dir no_recurrence*/
		    for(i = 1; i <= kmj; i++) {
			diff = ara[left+i]-ara[left+i-kmj];
			if (diff > 0.) {
			    dara[i][jp1] = ((dara[i+1][j]-dara[i][j])/diff)*kmj;
			}
		    }
		}
		bsplvb(ara,1,1,&ara[left],&left,biatx);
		darb[jp][lsofar] = dara[1][jp];
		for(jp1 = 2; jp1 <= jp; jp1++) {
		    bsplvb(ara,jp1,2,&ara[left],&left,biatx);
		    kmj = k+1-jp1;
		    sum = 0.;
		    for(i = 1; i <=jp1; i++) {
			sum += biatx[i]*dara[i][kmj];
			darb[kmj][lsofar] = sum;
		    }
		}
	    }
	}
    }
    *kptr = lsofar;
}

/*---------------------------------------------------------------------------*/
/*  calculates all nonzero beta-splines at *xptr  */
void bsplvb(double ara[],int jhigh,int index,double* xptr,
	    int* iptr, double arb[])
{
    int	ip, jp1, i;
    double  xp, saved, term;
    ip = *iptr;
    xp = *xptr;
    if (index == 1) {
	jbsp = 1;
	arb[1] = 1.;
    }
    while (jbsp < jhigh) {
	jp1 = jbsp+1;
	deltar[jbsp] = ara[ip+jbsp]-xp;
	deltal[jbsp] = xp-ara[ip+1-jbsp];
	saved = 0.;
	for (i = 1; i <= jbsp; i++) {
	    term = arb[i]/(deltar[i]+deltal[jp1-i]);
	    arb[i] = saved+deltar[i]*term;
	    saved = deltal[jp1-i]*term;
	}
	arb[jp1] = saved;
	jbsp++;
    }
}

/*---------------------------------------------------------------------------*/
/*  calculates and prints details of spline fit;
 *
 *  input:
 *     prfun = 0: no final results written to splres,
 *	       1: final results written to splres;
 *     grfun = 0: no first derivative calculated for plot,
 *           = 1: first derivative calculated for plot.
 */
void l2err(int prfun, int grfun)
{
    int	ll;
    double  err, errl2=0.0, a, dtau, ddtau;
    double  errm, dipn2av, di2av, d2av, dipndiav, expav, sum1, sum2;
    double  dev, small, big;

    small = 1e30; big = -1e30;

    /* v6.0 Combined check on correlation-corrected dws for n=nmin..nmax
       This code applies to a single ksi-value */

    sum1 = sum2 =0.0;
    acfa[0] = acfv[0] = 1.0;
	
    if (nmax >= ntau) nmax = ntau-1; /* restricted sums may not be empty */
	
    for (njan=nmin; njan<=nmax; njan++) {
	dipn2av = di2av = d2av = dipndiav = expav = 0.0;
	  
	for (ll = 1; ll <= ntau; ll++) {
	    ftau[ll] = ppvalu(brek,coef,&l,&k,&tau[ll],0);
	    /* All residuals are taken in normalized form : */
	    q[ll] = (ftau[ll]-gtau[ll])*sqrt(weight[ll]);
	    if (grfun == 1 && k != 1)
	    {
		deriv[ll] = ppvalu(brek,coef,&l,&k,&tau[ll],1);
		if (fabs(deriv[ll]) > dermax) dermax = fabs(deriv[ll]);
	    }
	    err = q[ll];
	    d2av += err*err;
	    if (ll > njan) {
		errm = q[ll-njan];
		dipn2av  += err*err;
		di2av    += errm*errm;
		dipndiav += err*errm;
		expav    += acffunc(acfindex,tau[ll]-tau[ll-njan],ksi);
	    }
	}
	    
	errl2 = d2av;
	d2av /= (double)ntau;
	dipn2av /= (double)(ntau-njan);
	di2av /= (double)(ntau-njan);
	dipndiav /= (double)(ntau-njan);
	expav /= (double)(ntau-njan);
	sum1 += ((dipn2av + di2av)/d2av);
	dev = (dipndiav/d2av - expav);
	sum2 += dev;
	if (dev < small) small = dev;
	if (dev > big)   big   = dev;
	acfa[njan] = dipndiav/d2av;
	acfv[njan] = expav;
    }
	  
    dws  = (sum1 - 2.0*sum2)/(double)(nmax-nmin+1);
    dws *= (1.0 - 1.0/(double)ntau);

    freed = ntau-km1-l;
    rms = sqrt((errl2/freed));
    acfdevmin = small;
    acfdevmax = big;

    if (prfun == 1) {
	for (ll = 1; ll <= ntau; ll++) {
	    a = ntau*weight[ll]/totalw; /* why this variable? */
	    dtau = ppvalu(brek,coef,&l,&k,&tau[ll],1);
	    ddtau = ppvalu(brek,coef,&l,&k,&tau[ll],2);
	    if (fullon && fpp) {
		fprintf(fpp, " %g %g %g %g %g %g %g\n", 
			tau[ll], gtau[ll], ftau[ll], 
			q[ll], weight[ll], dtau, ddtau);
	    }
	}
    }
}

/*---------------------------------------------------------------------------*/
/*  evaluates the jderiv-th derivative of a pp-function  */
double ppvalu(double ara[],double dara[][NMAX+1],
	      int* iptr,int* jptr,double* xptr,int jderiv)
{
    int	ip, jp, fmmjdr, i, ndummy, m;
    double  xp, h;
    ip = *iptr;
    jp = *jptr;
    xp = *xptr;
    ppvalue = 0.;
    fmmjdr = jp-jderiv;
    if (fmmjdr > 0) {
	interv(ara,&ip,&xp,&i,&ndummy);
	h = xp-ara[i];
	for (m = jp; m >= jderiv+1; m--) {
	    ppvalue = (ppvalue/fmmjdr)*h+dara[m][i];
	    fmmjdr--;
	}
    }
    return(ppvalue);
}

/*---------------------------------------------------------------------------*/
/*  locates a point within an increasing sequence of points  */
void interv(double ara[],int* iptr,double* xptr,int* jptr,int* kptr)
{
    int	ip, jp, kp, istep, middle, ilos=0;
    double	xp;
    ip = *iptr;
    xp = *xptr;
    kp = 10;
    ihi = ilo+1;

    if (ihi >= ip) {
	if (xp >= ara[ip]) 
	    kp = 1;
	else {
	    if (ip <= 1) 
		kp = -1;
	    else {
		ilo = ip-1;
		ihi = ip;
	    }
	}
    }
    if (kp == 10) {
	if (xp < ara[ihi]) {
	    if (xp >= ara[ilo]) 
		kp = 0;
	    else {
		istep = 1;
		while (ilo > 1 && xp < ara[ilo]) {
		    ihi = ilo;
		    ilo = ihi-istep;
		    istep *= 2;
		}
		if (ilo <= 1) {
		    ilo = 1;
		    if (xp < ara[1]) kp = -1;
		}
	    }
	}
	else {
	    istep = 1;
	    while (ihi < ip && xp > ara[ihi]) {
		ilo = ihi;
		ihi = ilo+istep;
		istep *= 2;
	    }
	    if (ihi >= ip)
	    {
		ihi = ip;
		if (xp > ara[ip]) kp = 1;
	    }
	}
	if (kp == 10) {
	    do {
		middle = (ilo+ihi)/2;
		if (xp >= ara[middle]) {
		    ilos = ilo;
		    ilo = middle;
		}
		else ihi = middle;
	    }
	    while (middle != ilos);
	}
    }
    if (kp == -1) 
	jp = 1;
    else {
	if (kp == 1) 
	    jp = ip;
	else {
	    kp = 0;
	    jp = ilo;
	}
    }
    *jptr = jp;
    *kptr = kp;
}

/*---------------------------------------------------------------------------*/
/* calculates percentages of lower (*xptr) and upper (*yptr)
 * percentages of x
 */

void dwtest(double x,double* xptr,double* yptr)
{
    int	i;
    double y, pin, ci, sc, scc, aa, pl, pu, xp, yp;

    pin = 4*atan(1.)/ntau;
    sc = scc = 0;
    for (i = 1; i < ntau-freed; i++) {
	ci = cos(pin*i);
	sc += ci;
	scc += ci*ci;
    }
    sc = sc/freed;
    aa = (double) freed*(freed+2)*(1-sc*sc)/(ntau-2-2*scc-2*freed*sc*sc)-1;
    pl = aa*(1-sc)/2;
    pu = aa*(1+sc)/2;
    y = x/4;
    xp = betai(pl,pu,y);
    yp = betai(pu,pl,y);
    *xptr = xp;
    *yptr = yp;
}

/*---------------------------------------------------------------------------*/
/* calculates Ix(a,b)	*/
double betai(double a, double b, double x)
{
    double bt, betai;
    if (x == 0 || x == 1)
	bt = 0;
    else
	bt = exp(lgamma(a+b)-lgamma(a)-lgamma(b)+a*log(x)+b*log(1-x));
    if (x < (a+1)/(a+b+2)) 
	betai = bt*betacf(a,b,x)/a;
    else
	betai = 1-bt*betacf(b,a,1-x)/b;
    return betai;
}

/*---------------------------------------------------------------------------*/
/* continued fraction for Ix(a,b) */
double betacf(double a,double b,double x)
{
    int	em, tem;
    double	aold, am, bm, az, qab, qap, qam, bz, d, ap, bp, app, bpp;
    aold = 0;
    em = 0;
    am = bm = az = 1;
    qab = a+b;
    qap = a+1;
    qam = a-1;
    bz = 1-qab*x/qap;
    while (fabs(az-aold) > 1e-7*fabs(az))
    {
	em++;
	tem = em+em;
	d = em*(b-em)*x/(qam+tem)/(a+tem);
	ap = az+d*am;
	bp = bz+d*bm;
	d = -(a+em)*(qab+em)*x/(a+tem)/(qap+tem);
	app = ap+d*az;
	bpp = bp+d*bz;
	aold = az;
	am = ap/bpp;
	bm = bp/bpp;
	az = app/bpp;
	bz = 1;
    }
    return(az);
}

/*---------------------------------------------------------------------------*/
/* calculates upper percentage points of chisquare of x with a degrees
   of freedom
*/
double chitest(double chi,int fr)
{
    double a, x, gamser, gln, gammcf;
    a = (double) fr/2.;
    x = chi/2;
    if (x < a+1) {
	gser(&gamser,a,x,&gln);
	gammcf = 1-gamser;
    }
    else 
	gcf(&gammcf,a,x,&gln);
    return(gammcf);
}

/*---------------------------------------------------------------------------*/
/* returns the incomplete gamma function p(a,x) evaluated by its
   series representation as *xptr; also returns ln(gamma(a)) as
   *yptr
   */
void gser(double* xptr,double a,double x,double* yptr)
{
    double	xp, yp, ap, sum, del;
    yp = lgamma(a);
    ap = a;
    sum = 1/a;
    del = sum;
    while (fabs(del) > fabs(sum)*1e-7)
    {
	ap++;
	del *= x/ap;
	sum += del;
    }
    xp = sum*exp(-x+a*log(x)-yp);
    *xptr = xp;
    *yptr = yp;
}

/*---------------------------------------------------------------------------*/
/* returns the incomplete gamma function q(a,x) evaluated by its
   continued fraction representation as *xptr; also returns ln(gamma(a))
   as *yptr
*/
void gcf(double* xptr,double a,double x,double* yptr)
{
    int	n;
    double	xp, yp, gold, b0, g, a0, b1, fac, a1, ana, anf;
    yp = lgamma(a);
    gold = b0 = 0.;
    a0 = b1 = fac = g = 1.;
    a1 = x;
    n = 1;
    while (fabs((g-gold)/g) > 1e-7) {
	if (a1 != 0 && n != 1) 
	    gold = g;
	ana = n-a;
	a0 = (a1+a0*ana)*fac;
	b0 = (b1+b0*ana)*fac;
	anf = n*fac;
	a1 = x*a0+anf*a1;
	b1 = x*b0+anf*b1;
	if (a1 != 0) {
	    fac = 1/a1;
	    g = b1*fac;
	}
	n++;
    }
    xp = exp(-x+a*log(x)-yp)*g;
    *xptr = xp;
    *yptr = yp;
}

/*---------------------------------------------------------------------------*/
/* returns the value of the assumed autocorrelation function of the
 * residuals in points spaced deltax apart. Normalized.
*/
double acffunc(int index,double deltax,double ksi)
{
    double a;
    if (ksi == 0.0) 
	return(0.0);
    switch(index) {
    case 1:
	return (exp(-deltax/ksi));
    case 2:
	return(exp(-deltax*deltax/ksi/ksi/2.0));
    case 3:
	a = 1.0-deltax/(2.0*ksi);
	return(a<0.0 ? 0.0 : a);
    case 4:
	a = 2.0*deltax/ksi;
	return(a==0.0 ? 1.0 : sin(a)/a);
    default:
	return 0.0;
    }
}

/*---------------------------------------------------------------------------*/
/* returns the log of the gamma function. Code supplied by Marcus Karolewski */
double lgamma(double xx)
{
    double x,y,tmp,ser;
    static double cof[6]={76.18009172947146,
			  -86.50532032941677,
			  24.01409824083091,
			  -1.231739572450155,
			  0.1208650973866179e-2,
			  0.5395239384953e-5};
    int j;
    y=x=xx;
    tmp=x+5.5;
    tmp -= (x+0.5)*log(tmp);
    ser=1.000000000190015;
    for (j=0;j<=5;j++) ser += cof[j]/++y;
    return -tmp+log(2.5066282746310005*ser/x);
}
