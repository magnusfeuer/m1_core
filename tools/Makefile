#
# Makefile for all tools
#

all: ../extern/lib/libssl.a
	(cd sqltool; make)
	(cd signtool; make)
	(cd pftool; make)
	(cd patchkey; make)
	(cd dbtool; make)

clean:
	(cd sqltool; make clean)
	(cd signtool; make clean)
	(cd pftool; make clean)
	(cd patchkey; make clean)
	(cd dbtool; make clean)

# Build extern if we don't have it
../extern/lib/libssl.a:
	(cd ../extern; make)

