
MODULES = \
	epic_app \
	epic_sup \
	epic_drv \
	epic \
	ewindow \
	ebitmap \
	epixmap \
	efont \
	egc \
	edict \
	ebackend \
	eimage \
	epic_csvfile \
	epic_ttf \
	epic_bdf \
	epic_bench \
	epic_test

include ../vsn.mk
VSN=$(EPIC_VSN)

APP_FILE= epic.app
APP_SRC= $(APP_FILE).src
APP_TARGET= ../ebin/$(APP_FILE)

override ERLC_FLAGS = -W -pa /Users/tony/work/jungerl/lib/erl_img/ebin


OBJS = $(MODULES:%=../ebin/%.beam)

TARGET_FILES = $(OBJS) $(APP_TARGET)

debug: ERLC_FLAGS += -Ddebug 

all: $(TARGET_FILES)

debug: all

depend:
	edep -MM -o ../ebin $(ERLC_FLAGS) $(MODULES:%=%.erl) > depend.mk

dialyze:
	dialyzer --src -o dia.out $(ERLC_FLAGS) -c $(MODULES:%=%.erl) 

clean:
	rm -f $(OBJS)

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);'  $< > $@

-include depend.mk

../ebin/%.beam:	%.erl
	erlc -o ../ebin $(ERLC_FLAGS) $<
