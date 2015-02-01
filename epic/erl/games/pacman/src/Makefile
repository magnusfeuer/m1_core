
MODULES = \
	pacman


include ../vsn.mk
VSN=$(PACMAN_VSN)

APP_FILE= pacman.app
APP_SRC= $(APP_FILE).src
APP_TARGET= ../ebin/$(APP_FILE)

ERL_IMG = ../../../../erl_img/ebin
EPIC    = ../../../../epic/ebin

override ERLC_FLAGS = -W -pa $(ERL_IMG) -pa $(EPIC)

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
