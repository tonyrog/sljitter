-ifndef(__SLJIT_NIF_HRL__).
-define(__SLJIT_NIF_HRL__, true).

%% Internal beam tag layout
%%
%%    00 = HEADER
%%    01 = LIST
%%    10 = BOXED
%%    11 = IMMED1
%%    11 = PRIMARY_MASK
%%
%%    0011 = PID
%%    0111 = PORT
%%    1011 = IMMED2
%%    1111 = SMALL
%%    1111 = IMMED1_MASK
%%
%%    001011 = ATOM
%%    011011 = CATCH
%%    101011 = ?
%%    111011 = NIL
%%    111111    IMMED2_MASK
%%
%%    000000    ARITY
%%    000100    BIN_MATCHSTATE
%%    001000    POS_BIG
%%    001100    NEG_BIG
%%    010000    REF
%%    010100    FUN
%%    011000    FLOAT
%%    011100    EXPORt
%%    100000    REFC_BINARY
%%    100100    HEAP_BINARY
%%    101000    SUB_BINARY
%%    101100    ---
%%    110000    EXTERNAL_PID
%%    110100    EXTERNAL_PORT
%%    111000    EXTERNAL_REF
%%    111100    MAP
%%    111111    HEADER_MASK
%%

-define(TAG_PRIMARY_SIZE,        2).
-define(TAG_PRIMARY_MASK,        2#11).
-define(TAG_PRIMARY_HEADER,      2#00).
-define(TAG_PRIMARY_LIST,        2#01).
-define(TAG_PRIMARY_BOXED,       2#10).
-define(TAG_PRIMARY_IMMED1,      2#11).

-define(TAG_IMMED1_SIZE,         4).
-define(TAG_IMMED1_MASK,         2#1111).
-define(TAG_IMMED1_PID,          2#0011).
-define(TAG_IMMED1_PORT,         2#0111).
-define(TAG_IMMED1_IMMED2,       2#1011).
-define(TAG_IMMED1_SMALL,        2#1111).

-define(TAG_IMMED2_SIZE,         6).
-define(TAG_IMMED2_MASK,         2#111111).
-define(TAG_IMMED2_ATOM,         2#001011).
-define(TAG_IMMED2_CATCH,        2#011011).
-define(TAG_IMMED2_,             2#101011).
-define(TAG_IMMED2_NIL,          2#111011).

-define(TAG_HEADER_SIZE,           6).
-define(TAG_HEADER_MASK,           2#1111_11).
-define(TAG_HEADER_ARITY,          2#0000_00).
-define(TAG_HEADER_FUN_REF,        2#0001_00).
-define(TAG_HEADER_POS_BIG,        2#0010_00).
-define(TAG_HEADER_NEG_BIG,        2#0011_00).
-define(TAG_HEADER_REF,            2#0100_00).
-define(TAG_HEADER_FUN,            2#0101_00).
-define(TAG_HEADER_FLOAT,          2#0110_00).
-define(TAG_HEADER_EXPORT,         2#0111_00).
-define(TAG_HEADER_HEAP_BITS,      2#1000_00).  
-define(TAG_HEADER_SUB_BITS,       2#1001_00).
-define(TAG_HEADER_BIN_REF,        2#1010_00).
-define(TAG_HEADER_MAP,            2#1011_00).
-define(TAG_HEADER_EXTERNAL_PID,   2#1100_00).
-define(TAG_HEADER_EXTERNAL_PORT,  2#1101_00).
-define(TAG_HEADER_EXTERNAL_REF,   2#1110_00).
-define(TAG_HEADER_EXTERNAL_MASK,  2#1100_00).

%% the only 0 around is the arity value for {} so it will not occure
%% as a return value. That goes for other HEADER tags as well
%% We may use HEADER + various code to code for exceptions etc
-define(INVALID_TERM, 0).

-endif.
