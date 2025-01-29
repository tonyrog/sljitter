#@BEGIN-C_SRC-DEFAULT-RULES@
ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
OS=$(shell uname -s)
CFLAGS=-MMD -MP -MF .$(notdir $<).d -I$(ERL_TOP)/usr/include
#@END-C_SRC-DEFAULT-RULES@

APP=sljit
CFLAGS+= -Wall -Wextra  -fno-common -O2 # -Wswitch-enum  -Wswitch-default
CFLAGS+= -I../external/sljit/sljit_src -DSLJIT_DEBUG=0 -DSLJIT_VERBOSE=1 -DSLJIT_CONFIG_AUTO=1

LDFLAGS+=-shared

OBJS = $(APP)_nif.o sljitLir.o
LIBS =

all: ../priv/$(APP)_nif.so

../priv/$(APP)_nif.so: $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) $(LIBS) -o $@

sljitLir.o: ../external/sljit/sljit_src/sljitLir.c
	$(CC) $(CFLAGS) -c -fPIC $<

%.o:	%.c
	$(CC) $(CFLAGS) -c -fPIC $<

%.o:	%.cpp
	$(CXX) $(CXXFLAGS) -c -fPIC $<

.%.d:	;

-include .*.d
