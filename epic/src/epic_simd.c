/*
 * Simd setup fucntions
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include "epic.h"
#include "epic_cpuid.h"
#include "epic_simd.h"

void (*ESimdCopy)(const u_int8_t* src, u_int8_t* dst, size_t n);

void (*ESimdFill32)(u_int8_t* dst, u_int32_t v, size_t n);

void (*ESimdAddBlendAreaRGBA32)(u_int8_t* src, int src_wb,
				u_int8_t* dst, int dst_wb,
				u_int8_t af, EPixel_t color,
				unsigned int width, 
				unsigned int height);
void (*ESimdAddBlendAreaARGB32)(u_int8_t* src, int src_wb,
				u_int8_t* dst, int dst_wb,
				u_int8_t af, EPixel_t color,
				unsigned int width, 
				unsigned int height);
void (*ESimdAddBlendAreaA8_RGBA32)(u_int8_t* src, int src_wb, 
				   u_int8_t* dst, int dst_wb,
				   u_int8_t af, EPixel_t color,
				   unsigned int width, 
				   unsigned int height);
void (*ESimdAddBlendAreaA8_ARGB32)(u_int8_t* src, int src_wb,
				   u_int8_t* dst, int dst_wb,
				   u_int8_t af, EPixel_t color,
				   unsigned int width, 
				   unsigned int height);

void (*ESimdAlphaAreaARGB32)(u_int8_t* src, int src_wb,
			     u_int8_t* dst, int dst_wb,
			     u_int8_t a, unsigned int width,
			     unsigned int height);
void (*ESimdAlphaAreaRGBA32)(u_int8_t* src, int src_wb,
			     u_int8_t* dst, int dst_wb,
			     u_int8_t a, unsigned int width,
			     unsigned int height);
void (*ESimdBlendAreaRGBA32)(u_int8_t* src, int src_wb, 
			     u_int8_t* dst, int dst_wb,
			     unsigned int width, unsigned int height);
void (*ESimdBlendAreaARGB32)(u_int8_t* src, int src_wb, 
			     u_int8_t* dst, int dst_wb,
			     unsigned int width, unsigned int height);
void (*ESimdFadeAreaRGBA32)(u_int8_t* src, int src_wb,
			    u_int8_t* dst, int dst_wb,
			    u_int8_t af, 
			    unsigned int width, unsigned int height);
void (*ESimdFadeAreaARGB32)(u_int8_t* src, int src_wb,
			    u_int8_t* dst, int dst_wb,
			    u_int8_t af, 
			    unsigned int width, unsigned int height);
void (*ESimdFillAreaBlendRGB24)(u_int8_t* dst,int dst_wb,
				unsigned int width, unsigned int height, 
				EPixel_t p);
void (*ESimdFillAreaBlendARGB32)(u_int8_t* dst,int dst_wb,
				 unsigned int width, 
				 unsigned int height, 
				 EPixel_t p);
void (*ESimdFillAreaBlendRGBA32)(u_int8_t* dst,int dst_wb,
				 unsigned int width, 
				 unsigned int height, 
				 EPixel_t p);


#define SIMD_FUNCTION_INIT(type) \
    ESimdCopy = ESimdCopy##type;				\
    ESimdFill32 = ESimdFill32##type;				\
    ESimdAddBlendAreaRGBA32 = ESimdAddBlendAreaRGBA32##type;	\
    ESimdAddBlendAreaARGB32 = ESimdAddBlendAreaARGB32##type;	\
    ESimdAddBlendAreaA8_RGBA32 = ESimdAddBlendAreaA8_RGBA32##type;	\
    ESimdAddBlendAreaA8_ARGB32 = ESimdAddBlendAreaA8_ARGB32##type;	\
    ESimdAlphaAreaARGB32 = ESimdAlphaAreaARGB32##type;			\
    ESimdAlphaAreaRGBA32 = ESimdAlphaAreaRGBA32##type;		\
    ESimdBlendAreaRGBA32 = ESimdBlendAreaRGBA32##type;		\
    ESimdBlendAreaARGB32 = ESimdBlendAreaARGB32##type;		\
    ESimdFadeAreaRGBA32  = ESimdFadeAreaRGBA32##type;		\
    ESimdFadeAreaARGB32  = ESimdFadeAreaARGB32##type;		\
    ESimdFillAreaBlendRGB24 = ESimdFillAreaBlendRGB24##type;	\
    ESimdFillAreaBlendARGB32 = ESimdFillAreaBlendARGB32##type;	\
    ESimdFillAreaBlendRGBA32 = ESimdFillAreaBlendRGBA32##type

static unsigned char   cpu_serial_number[64];
static size_t          cpu_serial_number_len = 0;
static char            cpu_vendor_name[64];
static size_t          cpu_vendor_name_len = 0;


//
// Return cpu serial number if available
// return 0 if not available otherwise the 
// length of the full serial number in bytes
//
int cpuSerialNumber(unsigned char* buf, size_t maxlen)
{
    int n = (cpu_serial_number_len > maxlen) ? maxlen : cpu_serial_number_len;
    memcpy(buf, cpu_serial_number, n);
    return cpu_serial_number_len;
}

//
// Return cpu vendor name if available
// retur 0 if not avaiable otherwise the 
// lenfth of the full cpu vendor name in bytes
//
int cpuVendorName(char* buf, size_t maxlen)
{
    int n = (cpu_vendor_name_len > maxlen) ? maxlen : cpu_vendor_name_len;
    memcpy(buf, cpu_vendor_name, n);
    return cpu_vendor_name_len;
}


#if defined(__i386__) || defined(__x86_64__)

static char* cpuid_feature_name[32] =
{
    "FPU", "VME",   "DE",      "PSE",
    "TSC", "MSR",   "PAE",     "MCE",
    "CX8", "APIC",  "B10",     "SEP",
    "MTRR",  "PGE", "MCA",     "CMOV",
    "PAT",   "PSE36", "PSN",   "CLFSH",
    "B20",   "DS",    "ACPI",  "MMX",
    "FXSR",  "SSE",   "SSE2",  "SS", 
    "HTT",   "TM",    "IA64",  "PBE" 
};

static void cpuid(int f, int *eax, int *ebx, int* ecx, int* edx)
{
    // FIXME add check if cpuid instruction is available,
    //  modern cpu's should have it so it's not very important right now.
    asm volatile ("mov %%ebx, %%esi\n\t" /* Save %ebx.  */
		  "cpuid\n\t"
		  "xchgl %%ebx, %%esi" /* Restore %ebx.  */
		  : "=a" (*eax), "=S" (*ebx), "=c" (*ecx), "=d" (*edx)
		  : "0" (f)
		  : "memory");
}

/* static void cpuid2(int f1, int f2, int *eax, int *ebx, int* ecx, int* edx) */
/* { */
/*     asm volatile ("mov %%ebx, %%esi\n\t" /\* Save %ebx.  *\/ */
/* 		  "cpuid\n\t" */
/* 		  "xchgl %%ebx, %%esi" /\* Restore %ebx.  *\/ */
/* 		  : "=a" (*eax), "=S" (*ebx), "=c" (*ecx), "=d" (*edx) */
/* 		  : "0" (f1), "c" (f2) */
/* 		  : "memory"); */
/* } */

/* static int cpuidMaxInputValue() */
/* { */
/*     int a,b,c,d; */
/*     cpuid(0,&a,&b,&c,&d); */
/*     return a; */
/* } */

// name must be at least 13 chars long 
static char* cpuidVendorName(char* name)
{
    int a,b,c,d;

    cpuid(0,&a,&b,&c,&d);

    *((int*)&name[0]) = b;
    *((int*)&name[4]) = d;
    *((int*)&name[8]) = c;
    name[12] = '\0';
    return name;
}

static int cpuidFeature()
{
    int a, b, c, d;

    cpuid(1, &a, &b, &c, &d);
    return d;
}

// Serial number is 12 bytes 
static int cpuidSerial(unsigned char* serial)
{
    int a, b, c, d;
    int i;
    cpuid(1, &a, &b, &c, &d);
    for (i = 0; i < 3; i++) {
	*serial++ = (a >> 24);
	a <<= 8;
    }
    cpuid(3, &a, &b, &c, &d);
    for (i = 0; i < 3; i++) {
	*serial++ = (d >> 24);
	d <<= 8;
    }
    for (i = 0; i < 3; i++) {
	*serial++ = (c >> 24);
	c <<= 8;
    }
    return 12;
} 

/* static int multiCoresPerProcPak() */
/* { */
/*     int a, b, c, d; */
/*     cpuid2(4,0, &a,&b,&c,&d); */
/*     return ((a & CPUID_CORES_PER_PROCPAK) >> 26) + 1; */
/* } */

/* static int getApicID() */
/* { */
/*     int a, b, c, d; */

/*     cpuid(1, &a, &b, &c, &d); */
/*     return (b & CPUID_LOCAL_APIC_ID) >> 24; */
/* } */

static int cpuidCacheLineSize()
{
    int a, b, c, d;

    cpuid(1, &a, &b, &c, &d);
    return ((b & CPUID_CLFUSH_SIZE) >> 8) << 3;
}
#endif


void ESimdInit(int accel)
{
    int feature = 0;
    int cacheline = 64;
#if defined(__i386__) || defined(__x86_64__)
    char* hex = "0123456789ABCDEF";
    int i;

    feature = cpuidFeature();
    cacheline = cpuidCacheLineSize();

    printf("Cpu: %s\r\n", cpuidVendorName(cpu_vendor_name));
    cpu_vendor_name_len = strlen(cpu_vendor_name);
    printf("Features: ");
    for (i = 0; i < 32; i++) {
	if ((1 << i) & feature)
	    printf("%s ", cpuid_feature_name[i]);
    }
    printf("\r\n");
    printf("CacheLineSize: %d\r\n", cacheline);

    if (feature & CPUID_PSN) {
	cpuidSerial(cpu_serial_number);
	cpu_serial_number_len = 12;
	printf("Serial: ");
	for (i = 0; i < 12; i++)
	    printf("%c%c", 
		   hex[(cpu_serial_number[i] >> 4)&0xf],
		   hex[cpu_serial_number[i] & 0xf]);
	printf("\r\n");
    }
    else {
	memset(cpu_serial_number, 0, sizeof(cpu_serial_number));
	cpu_serial_number_len = 0;
	printf("Serial: Not available\r\n");
    }
#endif
    printf("SIMD: Enable emu\r\n");
    SIMD_FUNCTION_INIT(_emu);
#if defined(__ppc__) || defined(__ppc64__)
#if defined(__VEC__) && defined(__ALTIVEC__)
    if (accel & EPIC_SIMD_ALTIVEC) {
	printf("SIMD: Enable altivec\r\n");
	SIMD_FUNCTION_INIT(_altivec);
    }
#endif
#endif


#if defined(__MMX__) && defined(USE_MMX)
    if ((feature & CPUID_MMX) &&
	((accel==EPIC_SIMD_AUTO) || (accel & EPIC_SIMD_MMX))) {
	printf("SIMD: Enable mmx\r\n");
	SIMD_FUNCTION_INIT(_mmx);
    }
#endif

#if defined(__SSE2__) && defined(USE_SSE2)
    if ((feature & CPUID_SSE2) &&
	((accel==EPIC_SIMD_AUTO) || (accel & EPIC_SIMD_SSE2))) {
	printf("SIMD: Enable sse2\r\n");
	SIMD_FUNCTION_INIT(_sse2);
    }
#endif
}
