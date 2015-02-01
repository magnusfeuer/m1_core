#
# Makefile for all tools
#

all: ddscomp/Makefile ../extern/lib/libssl.a
	(cd sqltool; make)
	(cd signtool; make)
	(cd ddscomp; make)
	(cd efnttool; make)
	(cd pftool; make)
	(cd patchkey; make)
	(cd dbtool; make)

clean:
	(cd sqltool; make clean)
	(cd signtool; make clean)
	(cd ddscomp; make clean)
	(cd efnttool; make clean)
	(cd pftool; make clean)
	(cd patchkey; make clean)
	(cd dbtool; make clean)

ddscomp/Makefile: ddscomp/Makefile.in ddscomp/configure
	(cd ddscomp; ./config.status)

ddscomp/configure: ddscomp/configure.ac
	(cd ddscomp; autoconf;./configure)

# Build extern if we don't have it
../extern/lib/libssl.a:
	(cd ../extern; make)

