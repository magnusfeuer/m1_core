//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

#ifndef __FFT_HH__
#define __FFT_HH__

#include <complex>

using namespace std;


class CFFT {
public:
    CFFT(size_t n);
    ~CFFT();

    void fft(complex<float>* x);
    void fft(complex<float>* x, complex<float>* y);
    void ifft(complex<float>* x);
    void rfft(float* x, float* y);

    void phase(complex<float>* x, float* y);
    void logMagnitude(complex<float>* x, float* y);
    void magnitude(complex<float>* x, float* y);

    void setRectangular(void);
    void setBartlett(void);
    void setHanning(void);
    void setHamming(void);
    void setBlackman(void);
    void setBlackmanHarris(void);

private:
    void reorder(complex<float>* x);
    void scale(complex<float>* x, float factor);
    void filter(complex<float>* x);
    void filter(complex<float>* x, complex<float>* y);
    void transform(complex<float>* x, bool inverse);
    


    size_t mSize;        // 2^mExp
    int    mExp;         // exponent
    complex<float>* mW;  // fourier kernel
    float* mH;           // filter factors if set
};

#endif
