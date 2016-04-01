
BINDIR = ../../../bin
TARGETS = $(BINDIR)/spline2 $(BINDIR)/evalsp $(BINDIR)/hpcurve
CFLAGS = -O2 -Wall

all: $(BINDIR) $(TARGETS)

$(BINDIR)/spline2:	spline2.o
	$(CC) -o $@ spline2.o -lm

$(BINDIR)/evalsp:	evalsp.o
	$(CC) -o $@ evalsp.o -lm

$(BINDIR)/hpcurve:	hpcurve.o
	$(CC) -o $@ hpcurve.o -lm

$(BINDIR):
	mkdir -p $(BINDIR)

clean: 
	rm -f $(BINDIR)/hpcurve  $(BINDIR)/spline2 $(BINDIR)/evalsp *.o
