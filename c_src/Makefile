#@BEGIN-C_SRC-DEFAULT-RULES@
ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
OS=$(shell uname -s)
CFLAGS=-MMD -MP -MF .$(notdir $<).d -I$(ERL_TOP)/usr/include
#@END-C_SRC-DEFAULT-RULES@

APP=sljit
CFLAGS+= -Wall -Wextra  -fno-common # -Wswitch-enum  -Wswitch-default
CFLAGS+= -I../external/sljit/sljit_src -DSLJIT_DEBUG=0 -DSLJIT_VERBOSE=1


-DSLJIT_CONFIG_EMULATOR=1 # -DSLJIT_CONFIG_AUTO=1

LDFLAGS+=-shared

OBJS = $(APP)_nif.o \
	sljitter_emulator.o \
	sljitter_x86_64.o
#	sljitter_x86_32.o \
#	sljitter_arm_v6.o \
#	sljitter_arm_v7.o \
#       sljitter_arm_thumb2.o \
#	sljitter_arm_64.o \

LIBS = -lffcall

all: ../priv/$(APP)_nif.so

../priv/$(APP)_nif.so: $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) $(LIBS) -o $@

sljitter_x86_64.o: ../external/sljit/sljit_src/sljitLir.c sljitter_backend.h
	$(CC) $(CFLAGS) -MF .sljitter_x86_64.d -DSLJIT_CONFIG_X86_64=1 -c -o$@ -fPIC sljitter_backend.c

sljitter_x86_32.o: ../external/sljit/sljit_src/sljitLir.c sljitter_backend.h
	$(CC) $(CFLAGS) -DSLJIT_CONFIG_X86_32=1 -c -o$@ -fPIC sljitter_backend.c

sljitter_arm_v6.o: ../external/sljit/sljit_src/sljitLir.c sljitter_backend.h
	$(CC) $(CFLAGS) -DSLJIT_CONFIG_ARM_V6=1 -c -o$@ -fPIC sljitter_backend.c

sljitter_arm_v7.o: ../external/sljit/sljit_src/sljitLir.c sljitter_backend.h
	$(CC) $(CFLAGS) -DSLJIT_CONFIG_ARM_V7=1 -c -o$@ -fPIC sljitter_backend.c

sljitter_arm_thumb2.o: ../external/sljit/sljit_src/sljitLir.c sljitter_backend.h
	$(CC) $(CFLAGS) -DSLJIT_CONFIG_ARM_THUMB2=1 -c -o$@ -fPIC sljitter_backend.c

sljitter_arm_64.o: ../external/sljit/sljit_src/sljitLir.c sljitter_backend.h
	$(CC) $(CFLAGS) -DSLJIT_CONFIG_ARM_64=1 -c -o$@ -fPIC sljitter_backend.c

sljitter_emulator.o: ../external/sljit/sljit_src/sljitLir.c sljitter_backend.h
	$(CC) $(CFLAGS) -MF .sljitter_emulator.d -DSLJIT_CONFIG_EMULATOR=1 -c -o$@ -fPIC sljitter_backend.c

%.o:	%.c
	$(CC) $(CFLAGS) -c -fPIC $<

%.o:	%.cpp
	$(CXX) $(CXXFLAGS) -c -fPIC $<

.%.d:	;

-include .*.d
