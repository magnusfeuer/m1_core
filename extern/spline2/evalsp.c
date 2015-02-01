/*
 *      Evalsp
 *
 *      Program for evaluating, analyzing, and manipulating a spline of which
 *	the so-called beta-spline representation (BS representation) is
 *      given. The BS-representation consists of the order k, the number 
 *	of intervals l, the values of the l+1 breakpoints, and the l+k-1 
 *	beta-spline coefficients.
 *	
 *	This program is intended to be used after running "spline2", which
 *	determines such a spline as a least-squares approximation to
 *	data points, and writes the above BS-representation to a file 
 *	named "splrep".
 *
 *  16 Jan 2002: Default number of output points increased from 250 to 2000.
 *   5 Feb 2002: Cosmetic changes to screen and evlsum layout.
 *
 ****	Syntax:
 *
 *	evalsp [filename1] [-x xbegin xend xstep] [-f xfile] [-v value]
 *	       [-e] [-i] [-a] [-F sbegin send sstep] [-h] [-b value]
 *
 ****	Command line options:
 *
 *	[filename1]		Name of file containing BS-representation,
 *				usually called "splrep".
 *				(Default=stdin).
 *
 *	[-x xbegin xend xstep]	Values in which the spline and its 
 *				derivatives are to be evaluated.
 *				This is essentially an interpolation operation.
 *				(Default: in 2000 equidistant steps between
 *				the first and last breakpoints, inclusive.
 *				This is convenient for plotting).
 *
 *				Note: the spline and its derivatives evaluated
 *				in the x-values of the original datapoints
 *				can be found in file "splres". This file is
 *				created when "spline2" is used with the -F
 *				option.
 *
 *	[-f xfile]		Name of file containing x-values (in
 *				increasing order) in which the spline and its
 *				derivatives are to be evaluated. This option
 *				is an alternative to the -x option. It allows
 *				the spline to be evaluated in x-values that
 *				belong, for instance, to another dataset.
 *
 *	[-v value]		Generates the x-value(s) in which the spline
 *				is equal to "value" (inverse interpolation,
 *				using Brent's method).
 *
 *	[-e]			Generates the x- and y-values of the minima
 *				and maxima of the spline.
 *
 *	[-i]		        Generates the x-, y- and dy/dx-values of the
 *				inflection points of the spline.
 *
 *	[-a]			Calculates the integral from the first to
 *				the last breakpoint.
 *
 *	[-F sbegin send sstep]  Evaluates the Fourier transform of the
 *				spline from sbegin to send in steps of sstep.
 *				Note: a direct fft is often to be preferred.
 *
 *	[-h]			Generates the x-values in which the spline
 *				reaches the "half-height" value, which
 *				is assumed to be the value halfway the
 *				highest maximum and the avarage value
 *				of all other minima and maxima. This is
 *				a very primitive method to determine the width
 *				of a single peak (on a possibly fluctuating
 *				background), but it works sometimes.
 *				Implies -e (and later: -v).
 *
 *	[-b value]		Specifies a baseline (background) value
 *				instead of having it calculated by the -h
 *				option.
 *
 *      [-A]                    Generate all files
 *
 *	Note concerning options -v, -e and -i: if the range between two 
 *	subsequent x-values contains more than one root, no root is found 
 *	in the case of an even number, and only one root is found in the case
 *	of an odd number of roots.
 *
 ****   Output:
 *
 *      File "evlres", containing x(i), S(x(i)), and all derivatives
 *		dj(S(i))/dxj; from j=1 to j=k-1. One line for each
 *		x-value.
 *
 *	File "evlsum", containing the results of options -v, -e, -i, -h and -a.
 *		This information is also written to stdout (the terminal).
 *
 *	File "evlfour", containing the values of the Fourier transform 
 *		as follows: s, Re(Ft), Im(Ft). One line for each
 *              s-value.
 *
 ****	Info:
 *
 *	Authors and owners of this program are:
 *		Barend J. Thijsse and Mark A. Hollanders,
 *              Computational Materials Science group (Com,ma,s)
 *		Physical Materials Science Division,
 *		Laboratory of Materials Science, 
 *		Delft University of Technology,
 *		Rotterdamseweg 137, 2628 AL  Delft,
 *			Phone: +31 15 278 2221
 *			Fax:   +31 15 278 6730
 *			E-mail: thijsse@stm.tudelft.nl
 *			URL: http://dutsm183.stm.tudelft.nl
 *
 */

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <ctype.h>


#define NMAX 2001
#define LMAX 1001
#define KMAX 24

void l2knts();
void bsplpp();
void bsplvb();
void interv();

double ppvalu();
double ppigr();
double zbrent();

int k;
int km1;
int l;
int n;
int left;
int jbsp;
int ihi;
int ilo = 1;
int hhon;
int nextremes;
int nomaxmax=1;
int firsttime=1;
int very_uncertain;

double brek[LMAX];
double bcoef[LMAX+KMAX];
double coef[KMAX][LMAX];
double ppvalue;
double deltal[KMAX];
double deltar[KMAX];
double halfheight;
double extrsum;
double maxmax;
double zerolevel;

FILE* fp_out = NULL;
FILE* fp_in  = NULL;
FILE* fps = NULL;
FILE* fpf = NULL;
FILE* fpx = NULL;

/* ---MAIN--- */
int main(int argc, char *argv[])
{
    register int i, j;
    int      xflag = 0;
    int      ntau;
    int      m;
    int      val = 0;
    int      ext = 0;
    int      inf = 0;
    int      are = 0;
    int      fou = 0;
    int	     background = 0;
    int      sum_file = 0;
    double   xbegin;
    double   xend;
    double   xstep = 0.0;
    double   y;
    double   x;
    double   t[LMAX+KMAX];
    double   b;
    double   bb = 0.0;
    double   a[KMAX];
    double   scrtch[KMAX][KMAX];
    double   c;
    double   a0 = 0.0;
    double   a1 = 0.0;
    double   a2 = 0.0;
    double   yder;
    double   ymax;
    double   xv[NMAX];
    double   value = 0.0;
    double   smin = 0.0;
    double   smax = 0.0;
    double   sstep = 0.0;
    double   pi;
    double   s;
    double   fourr;
    double   fouri;
    double   tpis;
    double   pref;
    double   argb;
    double   arga;
    double   bg = 0.0;
    char*    file;
    char*    infile = "STDIN";
    char*    outfile = "STDOUT";

    /* ---INITIALIZATION---*/
    fp_in  = stdin;
    fp_out = stdout;

    /* ---GET RUN TIME OPTION---*/
    if (argc > 1)
    {
	for (m=1; m<argc; m++) {
	    switch(*argv[m]) {
	    case '-':
		switch(*(argv[m]+1)) {
		case 'x':
		    xbegin = atof(argv[++m]);
		    xend   = atof(argv[++m]);
		    xstep  = atof(argv[++m]);
		    xflag = 1;
		    break;
		case 'v' :
		    val = 1;
		    value = atof(argv[++m]);
		    break;
		case 'b' : 
		    background = 1;
		    bg = atof(argv[++m]);
		    break;
		case 'e' :
		    ext = 1;
		    break;
		case 'h' :
		    ext = hhon = 1;
		    break;
		case 'i' :
		    inf = 1;
		    break;
		case 'a' : 
		    are = 1;
		    break;
		case 'F' : 
		    fou = 1;
		    smin = atof(argv[++m]);
		    smax = atof(argv[++m]);
		    sstep = atof(argv[++m]);
		    break; 
		case 'f':
		    xflag = 1;
		    file = argv[++m];
		    if ((fpx = fopen(file,"r")) == NULL) {
			fprintf(stderr, "cannot open %s (%s)\n", 
				file, strerror(errno));
			exit(1);
		    }
		    break;
		case 'o':
		    outfile = argv[++m];
		    if ((fp_out = fopen(outfile, "w")) == NULL) {
			fprintf(stderr, "cannot open %s (%s)\n", 
				outfile, strerror(errno));
			exit(1);
		    }
		    break;
		case 'S':
		    sum_file = 1;
		    break;
		default :
		    printf("\n Flag unknown: ignored.\n");
		    break;
		}
		break;
	    default : 
		infile = argv[m];
		if ((fp_in = fopen(infile,"r")) == NULL) {
		    fprintf(stderr, "\n cannot open %s\n", infile);
		    exit(1);
		}
	    }
	}
    }

    if (sum_file) {
	if ((fps = fopen("evlsum","w")) == NULL) {
	    fprintf(stderr, "cannot open evlsum (%s)\n", strerror(errno));
	    exit(1);
	}
    }
    if (fou) {
	if ((fpf = fopen("evlfour","w")) == NULL) {
	    fprintf(stderr, "cannot open evlfour (%s)\n", strerror(errno));
	    exit(1);
	}
    }

    /* ---PROCEED---*/
    fscanf(fp_in, "%d %d", &k, &l);
    if (k <= 1)
	val = 0;
    if (k <= 2)
	ext = 0;
    if (k <= 3)
	inf = 0;
    for (i = 1; i <= l+1; i++) 
	fscanf(fp_in, "%lf", brek+i);
    km1 = k-1;
    for (i = 1; i <= l+km1; i++)
	fscanf(fp_in, "%lf", bcoef+i);
    if (xflag == 0) {
	xbegin = brek[1];
	xend = brek[l+1];
	xstep = (xend-xbegin)/2000;
    }
    if (fpx) {
	i = 1;
	while (fscanf(fpx, "%lf", xv+i) != EOF && i < NMAX-1) i++;
	ntau = i-1;
	xbegin = xv[1];
	xend = xv[ntau];
    }
    else ntau = (xend-xbegin)/xstep+1;
    l2knts(brek,&l,&k,t,&n);
    bsplpp(t,bcoef,&n,&k,scrtch,brek,coef,&l);

relaunch:
    for (i = 1; i <= ntau; i++) {
	if (fpx) 
	    b = xv[i];
	else
	    b = xbegin+(i-1)*xstep;
	if (firsttime) 
	    fprintf(fp_out, " %.8g", b);
	for (j = 0; j <= km1; j++) {
	    a[j] = ppvalu(brek,coef,&l,&k,&b,j);
	    if (firsttime) 
		fprintf(fp_out," %.8g", a[j]);
	}
	if (firsttime) 
	    fprintf(fp_out, "\n");

	if (val == 1 && i > 1 && (a[0]-value)*(a0-value) <= 0. && a0 != 0) {
	    if (a[0]-value == 0.) x = b;
	    else x = zbrent(bb,b,0,value);
	    if (firsttime) {
		printf(" root...: x = %g \tS(x) = %g\n", x, value);
		if (fps)
		    fprintf(fps, " root...: x = %g \tS(x) = %g\n", x, value);
	    }
	    else {
		printf(" 1/2w.pt: x = %g \tS(x) = %g\n", x, value);
		if (fps)
		    fprintf(fps, " 1/2w.pt: x = %g \tS(x) = %g\n", x, value);
	    }
	}
	if (ext == 1 && i > 1 && a[1]*a1 <= 0. && a1 != 0 && a[2] != 0)
	{
	    if (a[1] == 0.) x = b;
	    else x = zbrent(bb,b,1,0);
	    ymax = ppvalu(brek,coef,&l,&k,&x,0);
	    if (a1 > 0.)
	    {
		nextremes++;
		if (hhon) {
		    extrsum += ymax;
		    if (ymax>maxmax || nomaxmax) 
		    {maxmax = ymax; nomaxmax=0;}
		}
		printf(" max....: x = %g \tS(x) = %g\n", x, ymax);
		if (fps) 
		    fprintf(fps, " max....: x = %g \tS(x) = %g\n", x, ymax);
	    }
	    else
	    {
		nextremes++;
		if (hhon) {
		    extrsum += ymax;
		    if (ymax>maxmax) maxmax = ymax;
		}
		printf(" min....: x = %g \tS(x) = %g\n", x, ymax);
		if (fps)
		    fprintf(fps, " min....: x = %g \tS(x) = %g\n", x, ymax);
	    }
	}
	if (inf == 1 && i > 1 && a[2]*a2 <= 0. && a2 != 0 && a[3] != 0)
	{
	    if (a[2] == 0.) x = b;
	    else x = zbrent(bb,b,2,0);
	    ymax = ppvalu(brek,coef,&l,&k,&x,0);
	    yder = ppvalu(brek,coef,&l,&k,&x,1);
	    if(yder>0.0){
		printf(" iflp...: x = %g \tS(x) = %g \tS'(x) = %g\n", x, ymax, yder);
		if (fps)
		    fprintf(fps, " iflp...: x = %g \tS(x) = %g \tS'(x) = %g\n", x, ymax, yder);
	    }
	    if(yder<0.0){
		printf(" iflp...: x = %g \tS(x) = %g \tS'(x) = %g\n", x, ymax, yder);
		if (fps)
		    fprintf(fps, " iflp...: x = %g \tS(x) = %g \tS'(x) = %g\n", x, ymax, yder);
	    }
	    if(yder==0.0){
		printf(" iflp...: x = %g \tS(x) = %g \tS'(x) = %g\n", x, ymax, yder);
		if (fps)
		    fprintf(fps, " iflp...: x = %g \tS(x) = %g \tS'(x) = %g\n", x, ymax, yder);
	    }
	}
	a0 = a[0];
	a1 = a[1];
	a2 = a[2];
	bb = b;
    }

    if (are == 1) {
	y = ppigr(brek,coef,&l,&k,&xbegin,&xend);
	printf(" area...[ x = %g \tx = %g ]  \tA = %g\n", xbegin, xend, y);
	if (fps) 
	    fprintf(fps, " area...[ x = %g \tx = %g ]  \tA = %g\n", xbegin, xend, y);
    }

    if (fou && fpf) {
	pi = 4*atan(1.);
	for (s = smin; s <= smax; s += sstep) {
	    if (fabs(s) < 1e-6*sstep) {
		fourr = ppigr(brek,coef,&l,&k,&xbegin,&xend);
		fouri = 0;
	    }
	    else {
		fourr = fouri = 0;
		tpis = 2*pi*s;
		pref = 1;
		for (j = 1; j <= k; j++) {
		    pref /= tpis;  	
		    c = ppvalu(brek,coef,&l,&k,&xbegin,j-1);
		    b = ppvalu(brek,coef,&l,&k,&xend,j-1);
		    arga = tpis*xbegin+.5*pi*j;
		    argb = tpis*xend+.5*pi*j;
		    fourr += pref*(c*cos(arga)-b*cos(argb));
		    fouri += pref*(b*sin(argb)-c*sin(arga));
		}
		for (i = 2; i <= l; i++) {
		    if (brek[i] > xbegin && brek[i] <= xend); /* BUGG!!!? */
		    {
			argb = tpis*brek[i]+.5*pi*k;
			b = coef[k][i]-coef[k][i-1];
			fourr += pref*b*cos(argb);
			fouri -= pref*b*sin(argb);
		    }
		}
	    }
	    fprintf(fpf, "%g %g %g\n", s, fourr, fouri);
	}
    }

    if (ext) {
	printf(" xtremes: N = %d\n", nextremes);
	if (fps)
	    fprintf(fps," xtremes: N = %d\n", nextremes);
    }

    if (hhon) {
	if (nextremes<1 || nomaxmax) {
	    printf(" half-height points cannot be determined\n");
	    if (fps)
		fprintf(fps,
			" half-height points cannot be determined\n");
	}
	else {
	    if (nextremes>1 && !background) {
		zerolevel = (extrsum-maxmax)/(nextremes-1);
		halfheight = zerolevel+(maxmax-zerolevel)/2.0;
	    }
	    else {
		if (!background) {
		    zerolevel = 0.0;
		    halfheight = maxmax/2.0;
		    very_uncertain = 1;
		}
		else {
		    zerolevel = bg;
		    halfheight = zerolevel+(maxmax-zerolevel)/2.0;
		}
	    }
	    printf(" bckgrnd:\t\tS(x) = %g (%slevel", zerolevel,
		   background ? "specified " : "estimated ");
	    if (very_uncertain) printf("; very uncertain");
	    printf(")\n");
	    if (fps)
		fprintf(fps," bckgrnd:\t\tS(x) = %g (%slevel", zerolevel,
			background ? "specified " : "estimated ");
	    if (very_uncertain && fps) fprintf(fps,"; very uncertain");
	    if (fps) fprintf(fps,")\n");
	    printf(" 1/2hght:\t\tS(x) = %g (%shalf-height", halfheight,
		   background ? "" : "estimated ");
	    if (very_uncertain) printf("; very uncertain");
	    printf(")\n");
	    if (fps) fprintf(fps, " 1/2hght:\t\tS(x) = %g (%shalf-height", halfheight,
			     background ? "" : "estimated ");
	    if (very_uncertain && fps) fprintf(fps,"; very uncertain");
	    if (fps) fprintf(fps,")\n");
	    /* reset options */
	    ext = hhon = inf = are = fou = firsttime = 0;
	    val = 1;
	    value = halfheight;
	    goto relaunch;
	}
    }

    if (fpx) fclose(fpx);
    if (fp_in != stdin) fclose(fp_in);
    if (fps) fclose(fps);
    if (fp_out != stdout) fclose(fp_out);
    if (fpf) fclose(fpf);

    exit(0);
}

/*------------------------------------------------------------------*/
double ppigr(ara,dara,iptr,jptr,xptr,yptr)
    int     *iptr, *jptr;
    double  *xptr, *yptr, ara[], dara[][LMAX];

/*  calculates integral from *xptr to *yptr  */
{
    int     i, j, right, ndummy;
    double  h, aa, bb, ppintgr;
    ppintgr = 0.;
    interv(ara,iptr,xptr,&left,&ndummy);
    h = *xptr-ara[left];
    bb = 0.;
    for (j = *jptr; j >= 1; j--) bb = (bb+dara[j][left])*h/j;
    interv(ara,iptr,yptr,&right,&ndummy);
    for (i = left; i < right; i++)
    {
	aa = 0.;
	h = ara[i+1]-ara[i];
	for (j = *jptr; j >= 1; j--)
	{
	    aa = (aa+dara[j][i])*h/j;
	}
	ppintgr += aa;
    }
    h = *yptr-ara[right];
    aa = 0.;
    for (j = *jptr; j >= 1; j--) aa = (aa+dara[j][right])*h/j;
    ppintgr = ppintgr+aa-bb;
    return(ppintgr);
}

/*------------------------------------------------------------------*/
void l2knts(ara,iptr,jptr,arb,kptr)
    int	*iptr, *jptr, *kptr;
    double  *ara, *arb;  

    /*  breakpoints to knots  */
{
    int	i;
    for (i = 1; i <= km1; i++) arb[i] = ara[1];
    for (i = 1; i <= *iptr; i++) arb[km1+i] = ara[i];
    n = km1+(*iptr);
    for (i = 1; i <= *jptr; i++) arb[*kptr+i] = ara[*iptr+1];
}

/*------------------------------------------------------------------*/
void bsplpp(ara,arb,iptr,jptr,dara,arc,darb,kptr)
    int	*iptr, *jptr, *kptr;
    double  ara[], arb[], arc[], dara[][KMAX], darb[][LMAX];

/*  converts spline to piecewise polynomial representation  */
{
    int	lsofar, j, i, jp1, kmj;
    double  diff, sum, biatx[KMAX];
    arc[1] = ara[*jptr];
    lsofar = 0;
    for (left = *jptr; left <= *iptr; left++)
    {
	if(ara[left+1] != ara[left])
	{
	    lsofar++;
	    arc[lsofar+1] = ara[left+1];
	    if (*jptr <= 1) darb[1][lsofar] = arb[left];
	    else
	    {
		for (i = 1; i <= *jptr; i++)
		{
		    dara[i][1] = arb[left-*jptr+i];
		}
		for (jp1 = 2; jp1 <= *jptr; jp1++)
		{
		    j = jp1-1;
		    kmj = k-j;
		    for(i = 1; i <= kmj; i++)
		    {
			diff = ara[left+i]-ara[left+i-kmj];
			if (diff > 0.)
			{
			    dara[i][jp1] = ((dara[i+1][j]-dara[i][j])/diff)*kmj;
			}
		    }
		}
		bsplvb(ara,1,1,&ara[left],&left,biatx);
		darb[*jptr][lsofar] = dara[1][*jptr];
		for(jp1 = 2; jp1 <= *jptr; jp1++)
		{
		    bsplvb(ara,jp1,2,&ara[left],&left,biatx);
		    kmj = k+1-jp1;
		    sum = 0.;
		    for(i = 1; i <=jp1; i++)
		    {
			sum += biatx[i]*dara[i][kmj];
			darb[kmj][lsofar] = sum;
		    }
		}
	    }
	}
    }
    *kptr = lsofar;
}

/*------------------------------------------------------------------*/
void bsplvb(ara,jhigh,index,xptr,iptr,arb)
    int	jhigh, index, *iptr;
    double  ara[], arb[], *xptr;

/*  calculates all nonzero beta-splines at *xptr  */
{
    int	jp1, i;
    double  saved, term;
    if (index == 1)
    {
	jbsp = 1;
	arb[1] = 1.;
    }
    while (jbsp < jhigh)
    {
	jp1 = jbsp+1;
	deltar[jbsp] = ara[*iptr+jbsp]-*xptr;
	deltal[jbsp] = (*xptr)-ara[*iptr+1-jbsp];
	saved = 0.;
	for (i = 1; i <= jbsp; i++)
	{
	    term = arb[i]/(deltar[i]+deltal[jp1-i]);
	    arb[i] = saved+deltar[i]*term;
	    saved = deltal[jp1-i]*term;
	}
	arb[jp1] = saved;
	jbsp++;
    }
}

/*------------------------------------------------------------------*/
double ppvalu(ara,dara,iptr,jptr,xptr,jderiv)
    int	*iptr, *jptr, jderiv;
    double  *xptr, ara[], dara[][LMAX];

/*  evaluates the jderiv-th derivative of a pp-function  */
{
    int	fmmjdr, i, ndummy, m;
    double  h;
    ppvalue = 0.;
    fmmjdr = *jptr-jderiv;
    if (fmmjdr > 0)
    {
	interv(ara,iptr,xptr,&i,&ndummy);
	h = *xptr-ara[i];
	for (m = *jptr; m >= jderiv+1; m--)
	{
	    ppvalue = (ppvalue/fmmjdr)*h+dara[m][i];
	    fmmjdr--;
	}
    }
    return(ppvalue);
}

/*------------------------------------------------------------------*/
void interv(ara,iptr,xptr,jptr,kptr)
    int	*iptr, *jptr, *kptr;
    double  *xptr, ara[];

/*  locates a point within an increasing sequence of points  */
{
    int	istep, middle, ilos = 0;
    *kptr = 10;
    ihi = ilo+1;
    if (ihi >= *iptr)
    {
	if (*xptr >= ara[*iptr]) *kptr = 1;
	else
	{
	    if (*iptr <= 1) *kptr = -1;
	    else
	    {
		ilo = *iptr-1;
		ihi = *iptr;
	    }
	}
    }
    if (*kptr == 10)
    {
	if (*xptr < ara[ihi])
	{
	    if (*xptr >= ara[ilo]) *kptr = 0;
	    else
	    {
		istep = 1;
		while (ilo > 1 && *xptr < ara[ilo])
		{
		    ihi = ilo;
		    ilo = ihi-istep;
		    istep *= 2;
		}
		if (ilo <= 1)
		{
		    ilo = 1;
		    if (*xptr < ara[1]) *kptr = -1;
		}
	    }
	}
	else
	{
	    istep = 1;
	    while (ihi < *iptr && *xptr > ara[ihi])
	    {
		ilo = ihi;
		ihi = ilo+istep;
		istep *= 2;
	    }
	    if (ihi >= *iptr)
	    {
		ihi = *iptr;
		if (*xptr > ara[*iptr]) *kptr = 1;
	    }
	}
	if (*kptr == 10)
	{
	    do
	    {
		middle = (ilo+ihi)/2;
		if (*xptr >= ara[middle])
		{
		    ilos = ilo;
		    ilo = middle;
		}
		else ihi = middle;
	    }
	    while (middle != ilos);
	}
    }
    if (*kptr == -1) *jptr = 1;
    else
    {
	if (*kptr == 1) *jptr = *iptr;
	else
	{
	    *kptr = 0;
	    *jptr = ilo;
	}
    }
}

/*------------------------------------------------------------------*/
/* using Brent's method, find the root of a function known to lie
 * between x1 and x2; the root returned as x3, will be refined until
 * its accuracy is smaller than 1e-8 times f(x1) or f(x2), depending
 * which one is largest;
 */
double zbrent(x1,x2,ind,value)
    int	ind;
    double	x1, x2, value;
{
    double y1;
    double y2;
    double y;
    double a = x1;
    double b = x2;
    double fa;
    double fb;
    double x3 = 0.0;
    double fc;
    double c = 0.0;
    double d = 0.0;
    double e = 0.0;
    double tol1;
    double xm;
    double s;
    double p;
    double q;
    double r;
    double eps = 1e-8;

    fa = ppvalu(brek,coef,&l,&k,&a,ind)-value;
    fb = ppvalu(brek,coef,&l,&k,&b,ind)-value;
    fc = fb;
    y1 = fabs(a);
    y2 = fabs(b);
    if (y1 > y2) y = y1;
    else y = y2;
    tol1 = 2*eps*y;
    while (x3 == 0) {
	if (fb*fc > 0) {
	    c = a;
	    fc = fa;
	    d = b-a;
	    e = d;
	}
	if (fabs(fc) < fabs(fb)) {
	    a = b;
	    b = c;
	    c = a;
	    fa = fb;
	    fb = fc;
	    fc = fa;
	}
	xm = .5*(c-b);
	if (fabs(xm) <= tol1 || fb == 0) x3 = b;
	else
	{
	    if (fabs(e) >= tol1 && fabs(fa) > fabs(fb))
	    {
		s = fb/fa;
		if (a == c)
		{
		    p = 2*xm*s;
		    q = 1-s;
		}
		else
		{
		    q = fa/fc;
		    r = fb/fc;
		    p = s*(2*xm*q*(q-r)-(b-a)*(r-1));
		    q = (q-1)*(r-1)*(s-1);
		}
		if (p > 0) q *= -1;
		p = fabs(p);
		y1 = 3*xm*q-fabs(tol1*q);
		y2 = fabs(e*q);
		if (y1 < y2) y = y1;
		else y = y2;
		if (2*p < y)
		{
		    e = d;
		    d = p/q;
		}
		else
		{
		    d = xm;
		    e = d;
		}
	    }
	    else
	    {
		d = xm;
		e = d;
	    }
	    a = b;
	    fa = fb;
	    if (fabs(d) > tol1) b += d;
	    else
	    {
		if (xm >= 0) y = tol1;
		else y = -1*tol1;
		b += y;
	    }
	    fb = ppvalu(brek,coef,&l,&k,&b,ind)-value;
	}
    }
    return(x3);
}
