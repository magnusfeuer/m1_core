/*
 * Calculate HP curve given  output from
 * spline2 | evalsp
 *
 *  timestamp(ms)  rpm  [ error value ..  ignored]
 *
 *  options:
 *         -g <ratio>            Gear ratio
 *         -w <weight>           Weight in kilograms
 *         -d <tire-diameter>    current tier diameter in mm
 *         -D <tire-dimension>   standard tier dimension in mm
 *
 * output options:
 *         -b <begin-rpm>
 *         -e <end-rpm>
 *         -s <step-rpm>
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>

const double const_watt_hp    = 745.699871;
const double const_hp_torque  = 33000.0 / (2*M_PI);

double parm_gear_ratio = 66.664871;
double parm_weight     = 1200.0;
double parm_standard_tire_dia = 733.8;
double parm_current_tire_dia  = 733.8;


typedef struct _sample {
    double t;      // time s from start
    double rpm;    // current rpm
    double v;      // calculated speed m/s
} Sample;

typedef struct _hp_sample {
    double rpm;
    double work;    // work in Joules
    double watts;   // power in Watts
    double hp;      // Power in hourse power
    double torque;  // Torque in lbt*ft
} HpSample;

double weight(double extra)
{
    return parm_weight + extra;
}

double watts_to_hp(double watts)
{
    return watts / const_watt_hp;
}

double rpm_to_hz(double rpm)
{
    return rpm / 60.0;
}

void make_hp(Sample* s0, Sample* s1, HpSample* hp)
{
    double v0 = s0->v;
    double v1 = s1->v;
    hp->rpm   = s1->rpm;
    hp->work  = (weight(0.0) / 2.0) * (v1*v1 - v0*v0);
    hp->watts = hp->work / (s1->t - s0->t);
    hp->hp    = watts_to_hp(hp->watts);
    hp->torque = (hp->hp*const_hp_torque)/rpm_to_hz(s1->rpm);
}

void usage()
{
    fprintf(stderr,
"Usage: hpcurve <options>\n"
" options:\n"
"         -g <ratio>            Gear ratio\n"
"         -w <weight>           Weight in kilograms\n"
"         -d <wheel-diameter>   current wheel diameter in mm\n"
"         -D <wheel-dimension>  standard wheel dimension in mm\n"
" output options:\n"
"         -b <begin-rpm>\n"
"         -e <end-rpm>\n"
"         -s <step-rpm>\n"
	);
    exit(1);
}

int main(int argc, char** argv)
{
    FILE* fp_in = stdin;
    FILE* fp_out = stdout;
    int lineno = 1;
    int samples = 0;
    char line[200];
    Sample s0;
    Sample s1;
    HpSample hp0;
    HpSample hp1;
    double ms;
    double rpm;
    double d1, d2;
    double tire_ratio;
    double begin_rpm = 0.0;
    double end_rpm = 0.0;
    double step_rpm = 0.0;
    double next_rpm = 0.0;
    int    interpolate = 0;
    int c;

    while((c = getopt(argc, argv, "g:w:d:D:b:e:s:")) != -1) {
	switch(c) {
	case 'g':
	    parm_gear_ratio = atof(optarg);
	    break;
	case 'w':
	    parm_weight = atof(optarg);
	    break;
	case 'd':
	    parm_current_tire_dia = atof(optarg);
	    break;
	case 'D':
	    parm_standard_tire_dia = atof(optarg);
	    break;
	case 'b':
	    begin_rpm = atof(optarg);
	    break;
	case 'e':
	    end_rpm = atof(optarg);
	    break;
	case 's':
	    step_rpm = atof(optarg);
	    interpolate = 1;
	    break;
	default:
	    usage();
	    break;
	}
    }

    // Tire ratio used to scale the caluclate speed (m/s) 
    if ((parm_standard_tire_dia > 0.0) && (parm_current_tire_dia > 0.0))
	tire_ratio = (parm_current_tire_dia) / (parm_standard_tire_dia);
    else
	tire_ratio = 1.0;
    next_rpm = begin_rpm;

    while (fgets(line,sizeof(line),fp_in)!=NULL) {
	int j;

	j = sscanf(line,"%lf %lf %lf %lf", &ms, &rpm, &d1, &d2);
	if (j<2) {
	    fprintf(stderr,
		    "Line %d has fewer than two numbers: skipped\n", lineno);
	    lineno++;
	    continue;
	}
	if (!samples && (rpm < begin_rpm))
	    continue;
	if ((rpm > end_rpm) && (end_rpm > 0.0)) {
	    exit(0);
	}
	s0 = s1;      // previous sample
	hp0 = hp1;    // previous hp value
	samples++;
	s1.t = ms / 1000.0;                // timestamp in seconds
	s1.rpm = rpm;
	// fixme adjust for wheel size
	s1.v = (1000.0 / 3600.0)*(rpm/parm_gear_ratio)*tire_ratio; // m/s
	//s1.v = (rpm/parm_gear_ratio)*tire_ratio; // m/s

	if (samples > 1) {
	    make_hp(&s0, &s1, &hp1);
	    /* Check for end condition
	       ms >= 2000.0 - wait a two sec before checking for end condition
	    */
	    if ((ms >= 2000.0) && ((hp1.hp < 0) || (s1.rpm+0.5 < s0.rpm))) {
		exit(0);
	    }
	    if (interpolate) {
		while ((next_rpm >= hp0.rpm) && (next_rpm < hp1.rpm)) {
		    double r = 1.0-((hp1.rpm - next_rpm)/(hp1.rpm-hp0.rpm));
		    double h = hp0.hp + (hp1.hp - hp0.hp)*r;
		    fprintf(fp_out, "%g %g\n", next_rpm, h);
		    next_rpm += step_rpm;
		}
	    }
	    else {
		fprintf(fp_out, "%g %g\n", hp1.rpm, hp1.hp);
	    }
	}
    }
    exit(0);
}
