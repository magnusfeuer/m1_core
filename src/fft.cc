//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

#include <complex>
#include <math.h>
#include "fft.hh"
#include <stdio.h>

//
// Create an fft instance of size n =  2^m  !!!
//
CFFT::CFFT(size_t n)
{
    int k = 1;
    int m = 0;
    int j;
    int le;
    double arg;
    complex<double> w;
    complex<double> w0;
    complex<float>* xj;

    // adjust n to power of two if needed
    while(k < (int) n) {
	k <<= 1;
	m++;
    }
    if ((int) n < k) {
	fprintf(stderr, "fft: size adjusted from %u to %d\n", n, k);
	mSize = k;
    }
    else
	mSize = n;
    mExp = m;

    le = n/2;
    // calculate kernel w for all n
    // [0 ... n/2-1 n/2 n/2+1 ... n-1]
    // [0...n/2-1] is used by fft and
    // [n-1... n/2+1] (reversed) used by inverse fft
    mW = new complex<float>[n];

    arg = M_PI / le;
    w = w0 = complex<double>(cos(arg), -sin(arg));

    xj = mW;
    for (j = 0; j < (int) n; j++) {
	*xj++ = complex<float>(w.real(), w.imag());
	w *= w0;
    }
    mH = NULL;
}

CFFT::~CFFT()
{
    delete [] mW;
    if (mH) delete [] mH;
}

//
// Reorder data according to bit numbers
// 
void CFFT::reorder(complex<float>* x)
{
    int j = 0;
    int i;

    for (i = 1; i < ((int)mSize-1); i++) {
	int k = mSize/2;

	while(k <= j) {
	    j = j-k;
	    k = k/2;
	}
	j = j + k;
	if (i < j) {
	    complex<float> a = x[j];
	    x[j] = x[i];
	    x[i] = a;
	}
    }
}

void CFFT::filter(complex<float>* x)
{
    if (!mH)
	return;
    for (int i = 0; i < (int) mSize; i++) x[i] *= mH[i];
}

void CFFT::filter(complex<float>* x, complex<float>* y)
{
    int i;
    if (!mH)
	for (i = 0; i< (int) mSize; i++) y[i] = x[i];
    else
	for (i = 0; i < (int) mSize; i++) y[i] = x[i]*mH[i];
}

void CFFT::setRectangular(void)
{
    if (!mH)
	return;
    for (int i = 0; i < (int) mSize; i++) mH[i] = 1.0;
}

void CFFT::setHamming(void)
{
    double arg = 2*M_PI/(mSize-1);
    if (!mH) mH = new float[mSize];
    for (int i = 0; i < (int)mSize; i++)
	mH[i] = 0.54 - 0.46*cos(arg*i);
}


void CFFT::setHanning(void)
{
    double arg = 2*M_PI/(mSize-1);
    if (!mH) mH = new float[mSize];
    for (int i = 0; i < (int)mSize; i++)
	mH[i] = 0.5 - 0.5*cos(arg*i);
}

void CFFT::setBartlett(void)
{
    double arg = 2.0/(mSize-1);
    int i;
    if (!mH) mH = new float[mSize];
    for (i = 0; i <= ((int)mSize-1)/2; i++)
	mH[i] = i*arg;
    for (; i < (int)mSize; i++)
	mH[i] = 2.0 - i*arg;
}

void CFFT::setBlackman(void)
{
    double arg = 2*M_PI/(mSize-1);
    if (!mH) mH = new float[mSize];
    for (int i = 0; i < (int)mSize; i++)
	mH[i] = 0.42 - 0.5*cos(arg*i) + 0.08*cos(2*arg*i);
}

void CFFT::setBlackmanHarris(void)
{
    double arg = 2*M_PI/(mSize-1);
    if (!mH) mH = new float[mSize];
    for (int i=0; i < (int)mSize; i++) 
	mH[i] = 0.35875 - 0.48829*cos(arg*i) +
	    0.14128*cos(2*arg*i) - 0.01168*cos(3*arg*i);
}


void CFFT::scale(complex<float>* x, float factor)
{
    int i;

    for (i = 0; i < (int)mSize; i++) 
	x[i] /= factor;
}

void CFFT::magnitude(complex<float>* x, float* y)
{
    for (int i = 0; i < (int)mSize; i++)
	y[i] = (x[i].real()*x[i].real()) + (x[i].imag()*x[i].imag());
}

void CFFT::logMagnitude(complex<float>* x, float* y)
{
    for (int i = 0; i < (int)mSize; i++)
	y[i] = 10*log10((x[i].real()*x[i].real()) + (x[i].imag()*x[i].imag()));
}

void CFFT::phase(complex<float>* x, float* y)
{
    for (int i = 0; i < (int)mSize; i++)
	y[i] = atan2(x[i].imag(),x[i].real());
}

// Run fast fourier transform (inline)
void CFFT::fft(complex<float>* x)
{
    filter(x);
    transform(x, false);
}

// Run fast fourier transform and place result in y
void CFFT::fft(complex<float>* x, complex<float>* y)
{
    filter(x, y);
    transform(y, false);
}

// Real input log magnitude output 
void CFFT::rfft(float* x, float* y)
{
    complex<float>* t = new complex<float>[mSize];

    for (int i = 0; i < (int) mSize; i++)
	t[i] = complex<float>(x[i], 0);

    filter(t);
    transform(t, false);
    logMagnitude(t, y);
    delete [] t;
}

// Run inverse fast fourier transform
void CFFT::ifft(complex<float>* x)
{
    // filter?
    transform(x, true);
    scale(x, 1.0 / mSize);
}

//
// Given a vector of complex data compute the fft
//
void CFFT::transform(complex<float>* x, bool inverse)
{
    int le = mSize;
    int l;
    int windex = inverse ? -1 : 1;
    int woffs  = -windex;
    complex<float>* wbase = inverse ? mW + (mSize-1) : mW;
    complex<float>* wptr;

    for (l = 0; l < mExp; l++) {
	int i, j;
	int le2 = le;

	le >>= 1;

	for (i = 0; i < (int) mSize; i += le2) {
	    complex<float> a = x[i] + x[i+le];
	    x[i+le] = x[i] - x[i+le];
	    x[i] = a;
	}

	wptr = wbase + windex - woffs;
	for (j = 1; j < le; j++) {
	    complex<float> wj = *wptr;

	    for (i = j; i < (int) mSize; i += le2) {
		complex<float> a = x[i] + x[i+le];
		x[i+le] = (x[i] - x[i+le])*wj;
		x[i]    = a;
	    }
	    wptr += windex;
	}
	windex <<= 1;
    }
    reorder(x);
}
