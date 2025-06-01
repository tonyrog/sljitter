# sljitter
Erlang api to sljit

# Deps

    https://github.com/tonyrog/sljit (emulator branch)
  
    libavcall1

# run test with emulator backend

    cd external/sljit
    make CFLAGS="-DSLJIT_CONFIG_STATIC=1 -DSLJIT_CONFIG_EMULATOR=1" bin/sljit_test
