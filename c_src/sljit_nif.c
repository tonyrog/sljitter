// sljit api

#include <stdio.h>
#include <stdint.h>
#include <memory.h>
#include <math.h>
#include <unistd.h>

#include "erl_nif.h"

#include "sljitLir.h"
// #include "sljitLir.c"

// #define NIF_TRACE
#define DEBUG

// #include "atom_hash.h"

#define UNUSED(a) ((void) a)

#define ATOM(name) atm_##name

#define DECL_ATOM(name) \
    ERL_NIF_TERM atm_##name = 0

#define LOAD_ATOM(name)			\
    atm_##name = enif_make_atom(env,#name)

#define LOAD_ATOM_STRING(env,name,string)	\
    atm_##name = enif_make_atom((env),string)

#define NIF_LIST \
    NIF("create_compiler",   0, nif_create_compiler) \
    NIF("get_platform_name", 0, nif_get_platform_name) \
    NIF("generate_code", 1, nif_generate_code) \
    NIF("register_code", 3, nif_register_code) \
    NIF("unregister_code", 2, nif_unregister_code) \
    NIF("code_info",  2, nif_code_info)	       \
    NIF("call_code", 1, nif_call_code)	       \
    NIF("call_code", 2, nif_call_code) \
    NIF("call_code", 3, nif_call_code) \
    NIF("call_code", 4, nif_call_code) \
    NIF("call_code", 5, nif_call_code) \
    NIF("emit_op0", 2, nif_emit_op0) \
    NIF("emit_op1", 6, nif_emit_op1) \
    NIF("emit_op2", 8, nif_emit_op2) \
    NIF("emit_op2u", 6, nif_emit_op2u) \
    NIF("emit_op2r", 7, nif_emit_op2r) \
    NIF("emit_shift_into", 7, nif_emit_shift_into) \
    NIF("emit_op_src", 4, nif_emit_src) \
    NIF("emit_op_dst", 4, nif_emit_dst) \
    NIF("emit_fop1", 6, nif_emit_fop1) \
    NIF("emit_fop2", 8, nif_emit_fop2) \
    NIF("emit_fop2r", 7, nif_emit_fop2r) \
    NIF("emit_fset32", 3, nif_emit_fset32) \
    NIF("emit_fset64", 3, nif_emit_fset64) \
    NIF("emit_fcopy", 4, nif_emit_fcopy) \
    NIF("emit_label", 1, nif_emit_label) \
    NIF("emit_jump", 2, nif_emit_jump) \
    NIF("emit_call", 3, nif_emit_call) \
    NIF("emit_cmp", 6, nif_emit_cmp) \
    NIF("emit_fcmp", 6, nif_emit_fcmp) \
    NIF("set_label", 2, nif_set_label) \
    NIF("set_target", 2, nif_set_target) \
    NIF("emit_ijump", 4, nif_emit_ijump) \
    NIF("emit_mjump", 4, nif_emit_mjump) \
    NIF("emit_icall", 5, nif_emit_icall) \
    NIF("emit_mcall", 5, nif_emit_mcall) \
    NIF("emit_enter", 6, nif_emit_enter) \
    NIF("set_context", 6, nif_set_context) \
    NIF("emit_return", 4, nif_emit_return) \
    NIF("emit_return_void", 1, nif_emit_return_void) \
    NIF("emit_return_to", 3, nif_emit_return_to) \
    NIF("emit_simd_op2", 6, nif_emit_simd_op2)


DECL_ATOM(sljit);
DECL_ATOM(ok);
DECL_ATOM(error);
DECL_ATOM(notsup);
DECL_ATOM(badarg);
DECL_ATOM(enoent);
DECL_ATOM(ealready);
DECL_ATOM(not_implemented);
DECL_ATOM(compiled);
DECL_ATOM(alloc_failed);
DECL_ATOM(ex_alloc_failed);
DECL_ATOM(unsupported);
DECL_ATOM(bad_argument);
DECL_ATOM(undefined);
// code info
DECL_ATOM(code);
DECL_ATOM(code_size);
DECL_ATOM(exec_offset);
DECL_ATOM(return_type);
DECL_ATOM(argc);
DECL_ATOM(arg_type);
// signature types
DECL_ATOM(void);
DECL_ATOM(word);
DECL_ATOM(word32);
DECL_ATOM(ptr);
DECL_ATOM(f64);
DECL_ATOM(f32);

// Declare all nif functions
#undef NIF
#ifdef NIF_TRACE
#define NIF(name, arity, func)						\
    static ERL_NIF_TERM func(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]); \
    static ERL_NIF_TERM trace##_##func##_##arity(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]);
#else
#define NIF(name, arity, func)						\
    static ERL_NIF_TERM func(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]);
#endif

NIF_LIST

#ifdef DEBUG
#include <stdio.h>
#define DBG(...) enif_fprintf(stderr, __VA_ARGS__)
#define BADARG(env) printf("matrix_nif.c: badarg line=%d\r\n", __LINE__), enif_make_badarg((env))
#else
#define DBG(...)
#define BADARG(env) enif_make_badarg((env))
#endif

#define EXCP_ERROR_N(env, arg_num, str)  raise_exception((env), ATOM(error),  (arg_num), (str), __FILE__, __LINE__)
#define EXCP_NOTSUP_N(env, arg_num, str) raise_exception((env), ATOM(notsup), (arg_num), (str), __FILE__, __LINE__)
#define EXCP_BADARG_N(env, arg_num, str) raise_exception((env), ATOM(badarg), (arg_num), (str), __FILE__, __LINE__)

ERL_NIF_TERM raise_exception(ErlNifEnv* env, ERL_NIF_TERM id, int arg_num, char* explanation, char* file, int line)
{
    ERL_NIF_TERM file_info, exception;
    char *error_msg = explanation;
    UNUSED(file);
    UNUSED(line);

    /* Make the data for exception */
    file_info = enif_make_new_map(env);
    enif_make_map_put(env, file_info,
                      enif_make_atom(env,"argument"),
                      enif_make_int(env, arg_num+1),  // +1 for erlang
                      &file_info);
    exception =
        enif_make_tuple3(env,
                         id,
                         file_info,
                         enif_make_string(env, error_msg, (ERL_NIF_LATIN1))
                         );
    return enif_raise_exception(env, exception);
}


static ErlNifResourceType* compiler_res;
static ErlNifResourceType* code_res;
static ErlNifResourceType* label_res;
static ErlNifResourceType* jump_res;

typedef struct {
    sljit_s32 arg_types;  // saved by enter and set_context    
    struct sljit_compiler* compiler;
} compiler_t;

typedef uintptr_t word_t;
typedef uint32_t  word32_t;
typedef uintptr_t ptr_t;
typedef double    float64_t;
typedef float     float32_t;

// signature type void only for return value
typedef enum {
    TYPE_VOID   = SLJIT_ARG_TYPE_RET_VOID, // 0
    TYPE_WORD   = SLJIT_ARG_TYPE_W,        // 1
    TYPE_WORD32 = SLJIT_ARG_TYPE_32,       // 2
    TYPE_PTR    = SLJIT_ARG_TYPE_P,        // 3
    TYPE_F64    = SLJIT_ARG_TYPE_F64,      // 4
    TYPE_F32    = SLJIT_ARG_TYPE_F32,      // 5
} sig_type_t;

// a few code prototypes 
typedef void     (*code0_t)(void);
typedef word_t   (*code1_t)(void);
typedef word32_t (*code2_t)(void);
typedef double   (*code4_t)(void);
typedef float    (*code5_t)(void);

typedef void   (*code01_t)(word_t);
typedef word_t (*code11_t)(word_t);
typedef float  (*code51_t)(word_t);

typedef void   (*code05_t)(float);
typedef word_t (*code15_t)(float);
typedef float  (*code55_t)(float);

typedef void   (*code03_t)(void*);
typedef word_t (*code13_t)(void*);
typedef float  (*code53_t)(void*);

typedef void   (*code011_t)(word_t,word_t);
typedef void   (*code031_t)(void*,word_t);
typedef word_t (*code111_t)(word_t,word_t);
typedef float  (*code511_t)(word_t,word_t);
typedef void   (*code051_t)(float,word_t);
typedef word_t (*code151_t)(float,word_t);
typedef float  (*code551_t)(float,word_t);
typedef word_t (*code131_t)(void*,word_t);
typedef float  (*code531_t)(void*,word_t);

typedef void   (*code015_t)(word_t,float);
typedef word_t (*code115_t)(word_t,float);
typedef float  (*code515_t)(word_t,float);
typedef void   (*code055_t)(float,float);
typedef word_t (*code155_t)(float,float);
typedef float  (*code555_t)(float,float);

typedef struct {
    sig_type_t ret;
    int argc;
    sig_type_t arg[4];
    void* addr;
    void* exec_allocator_data;
    sljit_sw exec_offset;
    sljit_uw code_size;
} code_t;

// export entry jump and calls are generate from &ent->addr !
typedef struct call_entry_t {
    void* addr;         // address of the function (must be top of structure)
    code_t* res;        // resource pointer (stored with keep)
    struct call_entry_t* next;  // keep track on all calls
    ERL_NIF_TERM mod;  // the "module" part of the function name
    ERL_NIF_TERM fun;  // the "function" part of the function name
} call_entry_t;

typedef struct {
    call_entry_t* first;
} nif_ctx_t;


// fixme - keep track on next jump resource!
typedef struct {
    struct sljit_jump* jump;
} jump_t;

// fixme - keep track on next label resource!
typedef struct {
    struct sljit_label* label;
} label_t;

#if 0
static int get_u32(ErlNifEnv* env, ERL_NIF_TERM term, sljit_u32* val)
{
    uint32_t v;
    if (!enif_get_uint(env, term, &v))
	return 0;
    *val = (sljit_u32) v;
    return 1;
}
static ERL_NIF_TERM make_uw32(ErlNifEnv* env, sljit_u32 val)
{
    return enif_make_uint(env, (unsigned) val);
}
#endif

static int get_s32(ErlNifEnv* env, ERL_NIF_TERM term, sljit_s32* val)
{
    int32_t v;
    if (!enif_get_int(env, term, &v))
	return 0;
    *val = (sljit_s32) v;
    return 1;
}

static ERL_NIF_TERM make_s32(ErlNifEnv* env, sljit_s32 val)
{
    return enif_make_int(env, (int) val);
}

static int get_sw(ErlNifEnv* env, ERL_NIF_TERM term, sljit_sw* val)
{
    int64_t v;
    if (!enif_get_int64(env, term, &v))
	return 0;
    *val = (sljit_sw) v;
    return 1;
}

static ERL_NIF_TERM make_sw(ErlNifEnv* env, sljit_sw val)
{
    return enif_make_int64(env, (int64_t) val);
}

static int get_uw(ErlNifEnv* env, ERL_NIF_TERM term, sljit_uw* val)
{
    uint64_t v;
    if (!enif_get_uint64(env, term, &v))
	return 0;
    *val = (sljit_uw) v;
    return 1;
}

static ERL_NIF_TERM make_uw(ErlNifEnv* env, sljit_uw val)
{
    return enif_make_uint64(env, (uint64_t) val);
}

static int get_float(ErlNifEnv* env, ERL_NIF_TERM term, float* fv)
{
    double v;
    if (!enif_get_double(env, term, &v))
	return 0;
    *fv = (float) v;
    return 1;
}

// Make faster (hash table)
static call_entry_t* lookup_code(ErlNifEnv* env,
				 ERL_NIF_TERM mod, ERL_NIF_TERM fun)
{
    nif_ctx_t* ctx = (nif_ctx_t*) enif_priv_data(env);
    call_entry_t* ent = ctx->first;
    while (ent != NULL) {
	if ((ent->mod == mod) && (ent->fun == fun)) 
	    return ent;
	ent = ent->next;
    }
    return NULL;
}

static int whereis_code(ErlNifEnv* env, ERL_NIF_TERM mod, ERL_NIF_TERM fun,
			code_t** codep)
{
    call_entry_t* ent = lookup_code(env, mod, fun);
    if (ent == NULL)
	return 0;
    if (codep != NULL)
	*codep = ent->res;
    return 1;
}

static int get_crp(ErlNifEnv* env, ERL_NIF_TERM term, code_t** codep)
{
    if (enif_get_resource(env, term, code_res, (void**)codep))
	return 1;
    else {
	const ERL_NIF_TERM* elems;
	int arity;
	if (!enif_get_tuple(env, term, &arity, &elems) || (arity != 2))
	    return 0;
	if (!enif_is_atom(env, elems[0]) || !enif_is_atom(env, elems[1]))
	    return 0;
	return whereis_code(env, elems[0], elems[1], codep);
    }
    return 0;
}

// how do we pass caller info here? thread local data?
static sljit_sw address_exception()
{
    enif_fprintf(stderr, "exception, code not loaded\n");
    return 0;
}


ERL_NIF_TERM nif_return(ErlNifEnv* env, int err)
{
    UNUSED(env);
    switch(err) {
    case SLJIT_SUCCESS: return ATOM(ok);
    case SLJIT_ERR_COMPILED: return ATOM(compiled);
    case SLJIT_ERR_ALLOC_FAILED: return ATOM(alloc_failed);
    case SLJIT_ERR_EX_ALLOC_FAILED: return ATOM(ex_alloc_failed);
    case SLJIT_ERR_UNSUPPORTED: return ATOM(unsupported);
    case SLJIT_ERR_BAD_ARGUMENT: return ATOM(bad_argument);
    default:
	enif_fprintf(stderr, "nif_return: unknown error code %d\r\n", err);
	return ATOM(undefined);
    }
}


ERL_NIF_TERM nif_create_compiler(ErlNifEnv* env, int argc,
				 const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    UNUSED(argv);
    struct sljit_compiler* cptr;
    void *allocator_data = NULL;
    compiler_t* cp;
    ERL_NIF_TERM term;

    cptr = sljit_create_compiler(allocator_data);
    cp = enif_alloc_resource(compiler_res, sizeof(compiler_t));
    cp->compiler = cptr;
    sljit_compiler_verbose(cptr, stdout);
    term = enif_make_resource(env,cp);
    enif_release_resource(cp);
    return term;
}

ERL_NIF_TERM nif_get_platform_name(ErlNifEnv* env, int argc,
				   const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    UNUSED(argv);
    const char* name = sljit_get_platform_name();
    return enif_make_string(env, name, ERL_NIF_LATIN1);
}

ERL_NIF_TERM nif_generate_code(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    void* code;
    ERL_NIF_TERM term;
    code_t* crp;
    unsigned rmask = 0x7;
    unsigned amask = 0x7;
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    code = sljit_generate_code(cp->compiler, 0, NULL);
    crp = enif_alloc_resource(code_res, sizeof(code_t));
    crp->addr = code;
    crp->exec_allocator_data = NULL;
    crp->ret = cp->arg_types & rmask;
    crp->argc = 0;
    if ((crp->arg[0] = (cp->arg_types >> SLJIT_ARG_SHIFT) & amask) != 0)
	crp->argc++;
    if ((crp->arg[1] = (cp->arg_types >> 2*SLJIT_ARG_SHIFT) & amask) != 0)
	crp->argc++;
    if ((crp->arg[2] = (cp->arg_types >> 3*SLJIT_ARG_SHIFT) & amask) != 0)
	crp->argc++;
    if ((crp->arg[3] = (cp->arg_types >> 4*SLJIT_ARG_SHIFT) & amask) != 0)
	crp->argc++;
    crp->exec_offset = sljit_get_executable_offset(cp->compiler);
    crp->code_size   = sljit_get_generated_code_size(cp->compiler);
    term = enif_make_resource(env,crp);
    enif_release_resource(crp);
    return term;
}

ERL_NIF_TERM nif_register_code(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    call_entry_t* ent;
    code_t* crp;

    if (!enif_get_resource(env, argv[0], code_res, (void**)&crp))
	return EXCP_BADARG_N(env, 0, "not code");
    if (!enif_is_atom(env, argv[1]))
	return EXCP_BADARG_N(env, 1, "not atom");
    if (!enif_is_atom(env, argv[2]))
	return EXCP_BADARG_N(env, 2, "not atom");
    // FIXME: atomic
    if ((ent = lookup_code(env, argv[1], argv[2])) == NULL) {
	nif_ctx_t* ctx = (nif_ctx_t*) enif_priv_data(env);
	ent = enif_alloc(sizeof(call_entry_t));
	enif_keep_resource(crp);
	ent->res  = crp;
	ent->addr = crp->addr;
	ent->mod = argv[1];
	ent->fun = argv[2];
	ent->next = ctx->first;
	ctx->first = ent;	
    }
    else {
	if (ent->addr != address_exception) // already registered?
	    return enif_make_tuple2(env, ATOM(error), ATOM(ealready));	    
	ent->addr = crp->addr;
    }
    return ATOM(ok);
}

ERL_NIF_TERM nif_unregister_code(ErlNifEnv* env, int argc,
				 const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    call_entry_t* ent;
    
    if (!enif_is_atom(env, argv[0]))
	return EXCP_BADARG_N(env, 0, "not atom");
    if (!enif_is_atom(env, argv[1]))
	return EXCP_BADARG_N(env, 1, "not atom");

    if ((ent = lookup_code(env, argv[0], argv[1])) == NULL) {
	return enif_make_tuple2(env, ATOM(error), ATOM(enoent));
    }
    // FIXME: atomic
    if (ent->res != NULL) {
	enif_release_resource(ent->res);
	ent->res = NULL;
	ent->addr = address_exception;
    }
    return ATOM(ok);    
}


ERL_NIF_TERM make_type(ErlNifEnv* env, sig_type_t type)
{
    UNUSED(env);
    switch(type) {
    case TYPE_VOID: return ATOM(void);
    case TYPE_WORD: return ATOM(word);
    case TYPE_WORD32: return ATOM(word32);
    case TYPE_PTR: return ATOM(ptr);
    case TYPE_F64: return ATOM(f64);
    case TYPE_F32: return ATOM(f32);
    default: return ATOM(undefined);
    }
}

ERL_NIF_TERM nif_code_info(ErlNifEnv* env, int argc,
			   const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    code_t* crp;

    if (!get_crp(env, argv[0], &crp))
	return EXCP_BADARG_N(env, 0, "not a code");    
    if (argv[1] == ATOM(code)) {
	ERL_NIF_TERM bin;
	unsigned char* dst;
	if ((crp->addr == NULL) || (crp->code_size == 0))
	    return ATOM(undefined);
	dst = enif_make_new_binary(env, crp->code_size, &bin);
	memcpy(dst, crp->addr, crp->code_size);
	return bin;
    }
    if (argv[1] == ATOM(code_size)) {
	return make_uw(env, crp->code_size);
    }
    if (argv[1] == ATOM(exec_offset)) {
	return make_sw(env, crp->exec_offset);
    }    
    if (argv[1] == ATOM(argc)) {
	return enif_make_int(env, crp->argc);
    }
    if (argv[1] == ATOM(return_type)) {
	return make_type(env, crp->ret);
    }
    if (enif_is_tuple(env, argv[1])) {
	int arity;
	const ERL_NIF_TERM* elems;
	int index;
	if (!enif_get_tuple(env, argv[1], &arity, &elems) || (arity != 2))
	    return EXCP_BADARG_N(env, 1, "not a pair");
	if (elems[0] != ATOM(arg_type))
	    return EXCP_BADARG_N(env, 1, "not arg_type");
	if (!enif_get_int(env, elems[1], &index) || (index < 1) || (index > 4))
	    return EXCP_BADARG_N(env, 1, "not arg index (1..4)");
	if (index > crp->argc) return ATOM(undefined);
	return make_type(env, crp->arg[index-1]);
    }
    return EXCP_BADARG_N(env, 1, "not a code info");
}

typedef union code_val {
    sljit_sw  sw;
    sljit_s32 s32;
    ErlNifBinary ptr;    
    double    f64;
    float     f32;
} code_val_t;


ERL_NIF_TERM nif_call_code(ErlNifEnv* env, int argc,
			   const ERL_NIF_TERM argv[])
{
    code_val_t ret;
    code_val_t arg[4];
    code_t* crp;
    int i;

    if (!get_crp(env, argv[0], &crp))
	return EXCP_BADARG_N(env, 0, "not a code");
    if (crp->addr == NULL)
	return EXCP_BADARG_N(env, 0, "not loaded");
    if (crp->argc != argc-1)
	return EXCP_BADARG_N(env, 0, "wrong number of arguments");

    // parse all arguments
    for (i = 0; i < argc-1; i++) {
	switch(crp->arg[i]) {
	case TYPE_WORD:
	    if (!get_sw(env, argv[i+1], &arg[i].sw))
		return EXCP_BADARG_N(env, i+1, "not an integer");
	    break;
	case TYPE_WORD32:
	    if (!get_s32(env, argv[i+1], &arg[i].s32))
		return EXCP_BADARG_N(env, i+1, "not an integer");
	    break;
	case TYPE_PTR:
	    if (!enif_inspect_iolist_as_binary(env, argv[i+1], &arg[i].ptr))
		return EXCP_BADARG_N(env, i+1, "not an iolist");
	    break;
	case TYPE_F64:
	    if (!enif_get_double(env, argv[i+1], &arg[i].f64))
		return EXCP_BADARG_N(env, i+1, "not a float");
	    break;
	case TYPE_F32:
	    if (!get_float(env, argv[i+1], &arg[i].f32))
		return EXCP_BADARG_N(env, i+1, "not a float");
	    break;
	case TYPE_VOID:
	default:
	    return EXCP_BADARG_N(env, i+1, "internal error");
	}
    }

    switch(crp->argc) {
    case 0:
	switch(crp->ret) {
	case TYPE_VOID:
	    ((code0_t) crp->addr)();
	    break;
	case TYPE_WORD:
	    ret.sw =((code1_t) crp->addr)();
	    break;
	case TYPE_WORD32:
	    ret.s32 =((code2_t) crp->addr)();
	    break;
	case TYPE_F32:
	    ret.f32 = ((code5_t) crp->addr)();
	    break;
	case TYPE_F64:
	    ret.f64 = ((code4_t) crp->addr)();
	    break;	    
	default:
	    return ATOM(not_implemented);
	}
	break;
	
    case 1:
	switch(crp->ret) {
	case TYPE_VOID: {
	    switch(crp->arg[0]) {
	    case TYPE_WORD:
		((code01_t) crp->addr)(arg[0].sw);
		break;
	    case TYPE_F32:
		((code05_t) crp->addr)(arg[0].f32);
		break;
	    case TYPE_PTR:
		((code03_t) crp->addr)(arg[0].ptr.data);
		break;
	    default:
		return ATOM(not_implemented);		
	    }
	    break;
	}
	case TYPE_WORD:
	    switch(crp->arg[0]) {
	    case TYPE_WORD:
		ret.sw = ((code11_t) crp->addr)(arg[0].sw);
		break;
	    case TYPE_F32:
		ret.sw = ((code15_t) crp->addr)(arg[0].f32);
		break;
	    case TYPE_PTR:
		ret.sw = ((code13_t) crp->addr)(arg[0].ptr.data);
		break;
	    default:
		return ATOM(not_implemented);		
	    }
	    break;
	case TYPE_F32:
	    switch(crp->arg[0]) {
	    case TYPE_WORD:
		ret.f32 = ((code51_t) crp->addr)(arg[0].sw);
		break;
	    case TYPE_F32:
		ret.f32 = ((code55_t) crp->addr)(arg[0].f32);
		break;
	    case TYPE_PTR:
		ret.f32 = ((code53_t) crp->addr)(arg[0].ptr.data);
		break;
	    default:
		return ATOM(not_implemented);		
	    }
	    break;
	default:
	    return ATOM(not_implemented);
	}
	break;
	
    case 2:
	switch(crp->ret) {
	case TYPE_VOID:
	    switch(crp->arg[0]+(crp->arg[1]<<4)) {
	    case TYPE_PTR+(TYPE_WORD<<1):
		((code031_t) crp->addr)(arg[0].ptr.data, arg[1].sw);
		break;
	    default:
		return ATOM(not_implemented);
	    }
	    break;
	case TYPE_WORD:
	    switch(crp->arg[0]+(crp->arg[1]<<4)) {
	    case TYPE_WORD+(TYPE_WORD<<4):
		ret.sw = ((code111_t) crp->addr)(arg[0].sw, arg[1].sw);
		break;
	    case TYPE_PTR+(TYPE_WORD<<4): 
		ret.sw = ((code131_t) crp->addr)(arg[0].ptr.data, arg[1].sw);
		break;
	    default:
		return ATOM(not_implemented);		
	    }
	    break;
	default:
	    return ATOM(not_implemented);
	}
	break;
    default:
	return ATOM(not_implemented);
    }

    switch(crp->ret) {
    case TYPE_VOID:
	return ATOM(ok);
    case TYPE_WORD:
	return make_sw(env, ret.sw);
    case TYPE_WORD32:
	return make_s32(env, ret.sw);	
    case TYPE_F32:
	return enif_make_double(env, ret.f32);
    case TYPE_F64:
	return enif_make_double(env, ret.f64);
    default:
	return ATOM(undefined);
    }
}

ERL_NIF_TERM nif_emit_op0(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    sljit_s32 op;
    sljit_s32 ret;

    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &op))
	return EXCP_BADARG_N(env, 1, "not an integer");
    ret = sljit_emit_op0(cp->compiler, op);
    return nif_return(env, ret);
}

ERL_NIF_TERM nif_emit_op1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    sljit_s32 ret;
    sljit_s32 op;    
    sljit_s32 dst; sljit_sw dstw;
    sljit_s32 src; sljit_sw srcw;    
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &op))
	return EXCP_BADARG_N(env, 1, "not an integer");
    if (!get_s32(env, argv[2], &dst))
	return EXCP_BADARG_N(env, 2, "not an integer");
    if (!get_sw(env, argv[3], &dstw))
	return EXCP_BADARG_N(env, 3, "not an integer");
    if (!get_s32(env, argv[4], &src))
	return EXCP_BADARG_N(env, 4, "not an integer");
    if (!get_sw(env, argv[5], &srcw))
	return EXCP_BADARG_N(env, 5, "not an integer");    
    ret = sljit_emit_op1(cp->compiler, op, dst, dstw, src, srcw);
    return nif_return(env, ret);
}

ERL_NIF_TERM nif_emit_op2(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    sljit_s32 ret;
    sljit_s32 op;
    sljit_s32 dst;
    sljit_sw dstw;
    sljit_s32 src1;
    sljit_sw src1w;
    sljit_s32 src2;
    sljit_sw src2w;

    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &op))
	return EXCP_BADARG_N(env, 1, "not an integer");
    if (!get_s32(env, argv[2], &dst))
	return EXCP_BADARG_N(env, 2, "not an integer");
    if (!get_sw(env, argv[3], &dstw))
	return EXCP_BADARG_N(env, 3, "not an integer");
    if (!get_s32(env, argv[4], &src1))
	return EXCP_BADARG_N(env, 4, "not an integer");
    if (!get_sw(env, argv[5], &src1w))
	return EXCP_BADARG_N(env, 5, "not an integer");
    if (!get_s32(env, argv[6], &src2))
	return EXCP_BADARG_N(env, 6, "not an integer");
    if (!get_sw(env, argv[7], &src2w))
	return EXCP_BADARG_N(env, 7, "not an integer");
    ret = sljit_emit_op2(cp->compiler, op, dst, dstw, src1, src1w, src2, src2w);
    return nif_return(env, ret);
}

ERL_NIF_TERM nif_emit_op2u(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    sljit_s32 ret;
    sljit_s32 op;
    sljit_s32 src1;
    sljit_sw src1w;
    sljit_s32 src2;
    sljit_sw src2w;

    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &op))
	return EXCP_BADARG_N(env, 1, "not an integer");
    if (!get_s32(env, argv[2], &src1))
	return EXCP_BADARG_N(env, 2, "not an integer");
    if (!get_sw(env, argv[3], &src1w))
	return EXCP_BADARG_N(env, 3, "not an integer");
    if (!get_s32(env, argv[4], &src2))
	return EXCP_BADARG_N(env, 4, "not an integer");
    if (!get_sw(env, argv[5], &src2w))
	return EXCP_BADARG_N(env, 5, "not an integer");
    ret = sljit_emit_op2u(cp->compiler, op, src1, src1w, src2, src2w);
    return nif_return(env, ret);
}

ERL_NIF_TERM nif_emit_op2r(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    sljit_s32 ret;
    sljit_s32 op;
    sljit_s32 dst_reg;
    sljit_s32 src1;
    sljit_sw src1w;
    sljit_s32 src2;
    sljit_sw src2w;
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &op))
	return EXCP_BADARG_N(env, 1, "not an integer");
    if (!get_s32(env, argv[2], &dst_reg))
	return EXCP_BADARG_N(env, 2, "not an integer");
    if (!get_s32(env, argv[3], &src1))
	return EXCP_BADARG_N(env, 3, "not an integer");
    if (!get_sw(env, argv[4], &src1w))
	return EXCP_BADARG_N(env, 4, "not an integer");
    if (!get_s32(env, argv[5], &src2))
	return EXCP_BADARG_N(env, 5, "not an integer");
    if (!get_sw(env, argv[6], &src2w))
	return EXCP_BADARG_N(env, 6, "not an integer");
    ret = sljit_emit_op2r(cp->compiler, op, dst_reg, src1, src1w, src2, src2w);
    return nif_return(env, ret);
}

ERL_NIF_TERM nif_emit_shift_into(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);    
    compiler_t* cp;
    sljit_s32 ret;    
    sljit_s32 op;
    sljit_s32 dst_reg;
    sljit_s32 src1_reg;
    sljit_s32 src2_reg;
    sljit_s32 src3;
    sljit_sw src3w;
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");

    if (!get_s32(env, argv[1], &op))
	return EXCP_BADARG_N(env, 1, "not an integer");
    if (!get_s32(env, argv[2], &dst_reg))
	return EXCP_BADARG_N(env, 2, "not an integer");
    if (!get_s32(env, argv[3], &src1_reg))
	return EXCP_BADARG_N(env, 3, "not an integer");
    if (!get_s32(env, argv[4], &src2_reg))
	return EXCP_BADARG_N(env, 4, "not an integer");
    if (!get_s32(env, argv[5], &src3))
	return EXCP_BADARG_N(env, 5, "not an integer");
    if (!get_sw(env, argv[6], &src3w))
	return EXCP_BADARG_N(env, 6, "not an integer");        
    ret = sljit_emit_shift_into(cp->compiler, op, dst_reg,
				src1_reg, src2_reg, src3, src3w);
    return nif_return(env, ret);    
}

ERL_NIF_TERM nif_emit_src(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    sljit_s32 ret;
    sljit_s32 op;
    sljit_s32 src;
    sljit_sw srcw;

    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &op))
	return EXCP_BADARG_N(env, 1, "not an integer");
    if (!get_s32(env, argv[2], &src))
	return EXCP_BADARG_N(env, 2, "not an integer");
    if (!get_sw(env, argv[3], &srcw))
	return EXCP_BADARG_N(env, 3, "not an integer");    
    ret = sljit_emit_op_src(cp->compiler, op, src, srcw);
    return nif_return(env, ret);    
}

ERL_NIF_TERM nif_emit_dst(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    sljit_s32 ret;
    sljit_s32 op;
    sljit_s32 dst;
    sljit_sw dstw;
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &op))
	return EXCP_BADARG_N(env, 1, "not an integer");
    if (!get_s32(env, argv[2], &dst))
	return EXCP_BADARG_N(env, 2, "not an integer");
    if (!get_sw(env, argv[3], &dstw))
	return EXCP_BADARG_N(env, 3, "not an integer");    
    ret = sljit_emit_op_dst(cp->compiler, op, dst, dstw);
    return nif_return(env, ret);    
}

ERL_NIF_TERM nif_emit_fop1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    sljit_s32 ret;
    sljit_s32 op;    
    sljit_s32 dst; sljit_sw dstw;
    sljit_s32 src; sljit_sw srcw;    
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &op))
	return EXCP_BADARG_N(env, 1, "not an integer");
    if (!get_s32(env, argv[2], &dst))
	return EXCP_BADARG_N(env, 2, "not an integer");
    if (!get_sw(env, argv[3], &dstw))
	return EXCP_BADARG_N(env, 3, "not an integer");
    if (!get_s32(env, argv[4], &src))
	return EXCP_BADARG_N(env, 4, "not an integer");
    if (!get_sw(env, argv[5], &srcw))
	return EXCP_BADARG_N(env, 5, "not an integer");
    ret = sljit_emit_fop1(cp->compiler, op, dst, dstw, src, srcw);
    return nif_return(env, ret);    
}

ERL_NIF_TERM nif_emit_fop2(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    sljit_s32 ret;
    sljit_s32 op;
    sljit_s32 dst;
    sljit_sw dstw;
    sljit_s32 src1;
    sljit_sw src1w;
    sljit_s32 src2;
    sljit_sw src2w;
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &op))
	return EXCP_BADARG_N(env, 1, "not a valid op2");
    if (!get_s32(env, argv[2], &dst))
	return EXCP_BADARG_N(env, 2, "not an integer");
    if (!get_sw(env, argv[3], &dstw))
	return EXCP_BADARG_N(env, 3, "not an integer");
    if (!get_s32(env, argv[4], &src1))
	return EXCP_BADARG_N(env, 4, "not an integer");
    if (!get_sw(env, argv[5], &src1w))
	return EXCP_BADARG_N(env, 5, "not an integer");
    if (!get_s32(env, argv[6], &src2))
	return EXCP_BADARG_N(env, 6, "not an integer");
    if (!get_sw(env, argv[7], &src2w))
	return EXCP_BADARG_N(env, 7, "not an integer");
    ret = sljit_emit_fop2(cp->compiler, op, dst, dstw, src1, src1w, src2, src2w);
    return nif_return(env, ret);
}

ERL_NIF_TERM nif_emit_fop2r(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    sljit_s32 ret;
    sljit_s32 op;
    sljit_s32 dstr;
    sljit_s32 src1;
    sljit_sw src1w;
    sljit_s32 src2;
    sljit_sw src2w;
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    
    if (!get_s32(env, argv[1], &op))
	return EXCP_BADARG_N(env, 1, "not a valid op2");
    if (!get_s32(env, argv[2], &dstr))
	return EXCP_BADARG_N(env, 2, "not an integer");
    if (!get_s32(env, argv[3], &src1))
	return EXCP_BADARG_N(env, 3, "not an integer");
    if (!get_sw(env, argv[4], &src1w))
	return EXCP_BADARG_N(env, 4, "not an integer");
    if (!get_s32(env, argv[5], &src2))
	return EXCP_BADARG_N(env, 5, "not an integer");
    if (!get_sw(env, argv[6], &src2w))
	return EXCP_BADARG_N(env, 6, "not an integer");
    ret = sljit_emit_fop2r(cp->compiler, op, dstr, src1, src1w, src2, src2w);
    return nif_return(env, ret);    
}

ERL_NIF_TERM nif_emit_fset32(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    sljit_s32 freg;
    sljit_f32 value;
    sljit_s32 ret;
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &freg))
	return EXCP_BADARG_N(env, 2, "not an integer");
    if (!get_float(env, argv[2], &value))
	return EXCP_BADARG_N(env, 3, "not a float");
    ret = sljit_emit_fset32(cp->compiler, freg, value);
    return nif_return(env, ret);
}

ERL_NIF_TERM nif_emit_fset64(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    sljit_s32 ret;    
    sljit_s32 freg;
    sljit_f64 value;    

    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &freg))
	return EXCP_BADARG_N(env, 2, "not an integer");
    if (!enif_get_double(env, argv[2], &value))
	return EXCP_BADARG_N(env, 3, "not a float");
    ret = sljit_emit_fset64(cp->compiler, freg, value);
    return nif_return(env, ret);    
}

ERL_NIF_TERM nif_emit_fcopy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    sljit_s32 ret;
    sljit_s32 op;
    sljit_s32 freg;
    sljit_s32 reg;

    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &op))
	return EXCP_BADARG_N(env, 1, "not an integer");
    if (!get_s32(env, argv[2], &freg))
	return EXCP_BADARG_N(env, 2, "not an integer");
    if (!get_s32(env, argv[3], &reg))
	return EXCP_BADARG_N(env, 3, "not an integer");
    ret = sljit_emit_fcopy(cp->compiler, op, freg, reg);
    return nif_return(env, ret);
}

ERL_NIF_TERM nif_emit_label(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    label_t* lbl;
    struct sljit_label* label;
    ERL_NIF_TERM term;
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if ((label = sljit_emit_label(cp->compiler)) == NULL)
	return nif_return(env, SLJIT_ERR_ALLOC_FAILED);
    lbl = enif_alloc_resource(label_res, sizeof(label_t));
    lbl->label = label;
    term = enif_make_resource(env,lbl);
    enif_release_resource(lbl);
    return term;
}

ERL_NIF_TERM nif_emit_jump(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    sljit_s32 type;
    struct sljit_jump* jump;
    jump_t* jmp;    
    ERL_NIF_TERM term;
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &type))
	return EXCP_BADARG_N(env, 1, "not an integer");
    if ((jump = sljit_emit_jump(cp->compiler, type)) == NULL)
	return nif_return(env, SLJIT_ERR_ALLOC_FAILED);
    jmp = enif_alloc_resource(jump_res, sizeof(jump_t));
    jmp->jump = jump;
    term = enif_make_resource(env,jmp);
    enif_release_resource(jmp);
    return term;
}

ERL_NIF_TERM nif_emit_call(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    sljit_s32 type;
    sljit_s32 arg_types;
    struct sljit_jump* jump;
    jump_t* jmp;
    ERL_NIF_TERM term;
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &type))
	return EXCP_BADARG_N(env, 1, "not an integer");
    if (!get_s32(env, argv[2], &arg_types))
	return EXCP_BADARG_N(env, 2, "not an integer");
    if ((jump = sljit_emit_call(cp->compiler, type, arg_types)) == NULL)
	return nif_return(env, SLJIT_ERR_ALLOC_FAILED);
    jmp = enif_alloc_resource(jump_res, sizeof(jump_t));
    jmp->jump = jump;
    term = enif_make_resource(env,jmp);
    enif_release_resource(jmp);
    return term;
}

ERL_NIF_TERM nif_emit_cmp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    sljit_s32 type;
    sljit_s32 src1;
    sljit_sw src1w;
    sljit_s32 src2;
    sljit_sw src2w;
    struct sljit_jump* jump;
    jump_t* jmp;
    ERL_NIF_TERM term;
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &type))
	return EXCP_BADARG_N(env, 1, "not an integer");    
    if (!get_s32(env, argv[2], &src1))
	return EXCP_BADARG_N(env, 4, "not an integer");
    if (!get_sw(env, argv[3], &src1w))
	return EXCP_BADARG_N(env, 5, "not an integer");
    if (!get_s32(env, argv[4], &src2))
	return EXCP_BADARG_N(env, 6, "not an integer");
    if (!get_sw(env, argv[5], &src2w))
	return EXCP_BADARG_N(env, 7, "not an integer");
    
    if ((jump = sljit_emit_cmp(cp->compiler, type, src1, src1w, src2, src2w)) == NULL)
	return nif_return(env, SLJIT_ERR_ALLOC_FAILED);    
    jmp = enif_alloc_resource(jump_res, sizeof(jump_t));
    jmp->jump = jump;
    term = enif_make_resource(env,jmp);
    enif_release_resource(jmp);
    return term;
}

ERL_NIF_TERM nif_emit_fcmp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    sljit_s32 type;
    sljit_s32 src1;
    sljit_sw src1w;
    sljit_s32 src2;
    sljit_sw src2w;
    struct sljit_jump* jump;
    jump_t* jmp;
    ERL_NIF_TERM term;
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &type))
	return EXCP_BADARG_N(env, 1, "not an integer"); 
    if (!get_s32(env, argv[2], &src1))
	return EXCP_BADARG_N(env, 2, "not an integer");
    if (!get_sw(env, argv[3], &src1w))
	return EXCP_BADARG_N(env, 3, "not an integer");
    if (!get_s32(env, argv[4], &src2))
	return EXCP_BADARG_N(env, 4, "not an integer");
    if (!get_sw(env, argv[5], &src2w))
	return EXCP_BADARG_N(env, 5, "not an integer");
    if ((jump = sljit_emit_fcmp(cp->compiler, type,
				src1, src1w, src2, src2w)) == NULL)
	return nif_return(env, SLJIT_ERR_ALLOC_FAILED);
    jmp = enif_alloc_resource(jump_res, sizeof(jump_t));
    jmp->jump = jump;
    term = enif_make_resource(env,jmp);
    enif_release_resource(jmp);
    return term;    
}

ERL_NIF_TERM nif_set_label(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    jump_t* jmp;
    label_t* lbl;
    
    if (!enif_get_resource(env, argv[0], jump_res, (void**)&jmp))
	return EXCP_BADARG_N(env, 0, "not a jump");
    if (!enif_get_resource(env, argv[1], label_res, (void**)&lbl))
	return EXCP_BADARG_N(env, 1, "not a label");
    sljit_set_label(jmp->jump, lbl->label);
    return ATOM(ok);
}

ERL_NIF_TERM nif_set_target(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    jump_t* jmp;
    sljit_uw target;
    
    if (!enif_get_resource(env, argv[0], jump_res, (void**)&jmp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_uw(env, argv[1], &target))
	return EXCP_BADARG_N(env, 1, "not an unsigned integer");
    sljit_set_target(jmp->jump, target);
    return ATOM(ok);    
}

ERL_NIF_TERM nif_emit_ijump(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    sljit_s32 ret;
    sljit_s32 type;
    sljit_s32 src;
    sljit_sw srcw;
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &type))
	return EXCP_BADARG_N(env, 1, "not an integer");
    if (!get_s32(env, argv[2], &src))
	return EXCP_BADARG_N(env, 2, "not an integer");
    if (!get_sw(env, argv[3], &srcw))
	return EXCP_BADARG_N(env, 3, "not an integer");    
    ret = sljit_emit_ijump(cp->compiler, type, src, srcw);
    return nif_return(env, ret);
}


ERL_NIF_TERM nif_emit_mjump(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    sljit_s32 ret;
    sljit_s32 type;
    call_entry_t* ent;    
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &type))
	return EXCP_BADARG_N(env, 1, "not an integer");
    if (!enif_is_atom(env, argv[2]))
	return EXCP_BADARG_N(env, 2, "not an atom");
    if (!enif_is_atom(env, argv[3]))
	return EXCP_BADARG_N(env, 3, "not an atom");

    if ((ent = lookup_code(env, argv[2], argv[3])) == NULL) {
	// create "export" entry
	nif_ctx_t* ctx = (nif_ctx_t*) enif_priv_data(env);
	code_t* crp;	
	ent = enif_alloc(sizeof(call_entry_t));
	crp = enif_alloc_resource(code_res, sizeof(code_t));
	crp->addr = NULL;
	crp->exec_allocator_data = NULL;
	crp->arg[0] = 0;
	crp->arg[1] = 0;
	crp->arg[2] = 0;
	crp->arg[3] = 0;
	crp->argc = 0;
	enif_keep_resource(crp);
	ent->res  = crp;
	ent->addr = address_exception;
	ent->mod = argv[2];
	ent->fun = argv[3];
	ent->next = ctx->first;
	ctx->first = ent;
    }
    else {
	// maybe warn if the target is a function
	// with arguments etc.
    }
    ret = sljit_emit_ijump(cp->compiler, type, SLJIT_MEM0(), (sljit_sw) ent);
    return nif_return(env, ret);
}

ERL_NIF_TERM nif_emit_icall(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    sljit_s32 ret;
    sljit_s32 type;
    sljit_s32 arg_types;
    sljit_s32 src;
    sljit_sw srcw;
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &type))
	return EXCP_BADARG_N(env, 1, "not an integer");
    if (!get_s32(env, argv[2], &arg_types))
	return EXCP_BADARG_N(env, 2, "not an integer");
    if (!get_s32(env, argv[3], &src))
	return EXCP_BADARG_N(env, 4, "not an integer");
    if (!get_sw(env, argv[4], &srcw))
	return EXCP_BADARG_N(env, 5, "not an integer");
    ret = sljit_emit_icall(cp->compiler, type, arg_types, src, srcw);    
    return nif_return(env, ret);
}

ERL_NIF_TERM nif_emit_mcall(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    code_t* crp;
    sljit_s32 ret;
    sljit_s32 type;
    sljit_s32 arg_types;
    unsigned rmask = 0x7;
    unsigned amask = 0x7;
    call_entry_t* ent;
    int cnt;
	
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &type))
	return EXCP_BADARG_N(env, 1, "not an integer");
    if (!get_s32(env, argv[2], &arg_types))
	return EXCP_BADARG_N(env, 2, "not an integer");
    if (!enif_is_atom(env, argv[3]))
	return EXCP_BADARG_N(env, 4, "not an atom");
    if (!enif_is_atom(env, argv[4]))
	return EXCP_BADARG_N(env, 5, "not an atom");
    
    if ((ent = lookup_code(env, argv[3], argv[4])) == NULL) {
	// create "export" entry
	nif_ctx_t* ctx = (nif_ctx_t*) enif_priv_data(env);
	ent = enif_alloc(sizeof(call_entry_t));
	crp = enif_alloc_resource(code_res, sizeof(code_t));
	crp->addr = NULL;
	crp->exec_allocator_data = NULL;
	crp->ret = arg_types & rmask;
	cnt = 0;
	if ((crp->arg[0] = (arg_types >> SLJIT_ARG_SHIFT) & amask) != 0)
	    cnt++;
	if ((crp->arg[1] = (arg_types >> 2*SLJIT_ARG_SHIFT) & amask) != 0)
	    cnt++;
	if ((crp->arg[2] = (arg_types >> 3*SLJIT_ARG_SHIFT) & amask) != 0)
	    cnt++;
	if ((crp->arg[3] = (arg_types >> 4*SLJIT_ARG_SHIFT) & amask) != 0)
	    cnt++;
	crp->argc = cnt;
	enif_keep_resource(crp);
	ent->res  = crp;
	ent->addr = address_exception;
	ent->mod = argv[3];
	ent->fun = argv[4];
	ent->next = ctx->first;
	ctx->first = ent;
    }
    else {
	sig_type_t arg;
	sig_type_t ret;
	int i;
	// entry found - check that it matches the call
	crp = ent->res;
	ret = (arg_types & rmask);
	if (ret != crp->ret)
	    return EXCP_BADARG_N(env, 4, "bad return type");
	cnt = 0;
	for (i = 0; i < 4; i++) {
	    if ((arg = (arg_types >> (i+1)*SLJIT_ARG_SHIFT) & amask) != 0)
		cnt++;
	    if (arg != crp->arg[i])
		return EXCP_BADARG_N(env, 4, "arg bad type");
	}
	if (crp->argc != cnt)
	    return EXCP_BADARG_N(env, 4, "wrong number of arguments");	
    }
    DBG("mcall: %T:%T/%d type=%x, argtypes=%x, addr=%p",
	ent->mod, ent->fun, crp->argc, type, arg_types, (void*) ent);
    
    ret = sljit_emit_icall(cp->compiler, type, arg_types,
			   SLJIT_MEM0(), (sljit_sw) ent);
    return nif_return(env, ret);
}

ERL_NIF_TERM nif_emit_enter(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    sljit_s32 options;
    sljit_s32 arg_types;
    sljit_s32 scratches;
    sljit_s32 saveds;
    sljit_s32 local_size;
    compiler_t* cp;
    sljit_s32 ret;
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &options))
	return EXCP_BADARG_N(env, 1, "not an integer");
    if (!get_s32(env, argv[2], &arg_types))
	return EXCP_BADARG_N(env, 2, "not an integer");
    if (!get_s32(env, argv[3], &scratches))
	return EXCP_BADARG_N(env, 3, "not an integer");
    if (!get_s32(env, argv[4], &saveds))
	return EXCP_BADARG_N(env, 4, "not an integer");
    if (!get_s32(env, argv[5], &local_size))
	return EXCP_BADARG_N(env, 5, "not an integer");
    ret = sljit_emit_enter(cp->compiler, options, arg_types, scratches,
			   saveds, local_size);
    cp->arg_types = arg_types;
    return nif_return(env, ret);
}

ERL_NIF_TERM nif_set_context(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    sljit_s32 options;
    sljit_s32 arg_types;
    sljit_s32 scratches;
    sljit_s32 saveds;
    sljit_s32 local_size;
    compiler_t* cp;
    sljit_s32 ret;
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &options))
	return EXCP_BADARG_N(env, 1, "not an integer");
    if (!get_s32(env, argv[2], &arg_types))
	return EXCP_BADARG_N(env, 2, "not an integer");
    if (!get_s32(env, argv[3], &scratches))
	return EXCP_BADARG_N(env, 3, "not an integer");
    if (!get_s32(env, argv[4], &saveds))
	return EXCP_BADARG_N(env, 4, "not an integer");
    if (!get_s32(env, argv[5], &local_size))
	return EXCP_BADARG_N(env, 5, "not an integer");
    ret = sljit_set_context(cp->compiler, options, arg_types, scratches,
			    saveds, local_size);
    cp->arg_types = arg_types;    
    return nif_return(env, ret);
}


ERL_NIF_TERM nif_emit_return(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    sljit_s32 op;
    sljit_s32 src;
    sljit_sw srcw;
    compiler_t* cp;
    sljit_s32 ret;
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &op))
	return EXCP_BADARG_N(env, 1, "not an integer");
    if (!get_s32(env, argv[2], &src))
	return EXCP_BADARG_N(env, 2, "not an integer");
    if (!get_sw(env, argv[3], &srcw))
	return EXCP_BADARG_N(env, 3, "not an integer");
    ret = sljit_emit_return(cp->compiler, op, src, srcw);
    return nif_return(env, ret);
}

ERL_NIF_TERM nif_emit_return_void(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    sljit_s32 ret;
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    ret = sljit_emit_return_void(cp->compiler);
    return nif_return(env, ret);
}

ERL_NIF_TERM nif_emit_return_to(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    sljit_s32 src;
    sljit_sw srcw;
    compiler_t* cp;
    sljit_s32 ret;
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &src))
	return EXCP_BADARG_N(env, 1, "not an integer");
    if (!get_sw(env, argv[2], &srcw))
	return EXCP_BADARG_N(env, 2, "not an integer");
    ret = sljit_emit_return_to(cp->compiler, src, srcw);
    return nif_return(env, ret);
}

ERL_NIF_TERM nif_emit_simd_op2(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    sljit_s32 ret;
    sljit_s32 type;
    sljit_s32 dst_vreg; 
    sljit_s32 src1_vreg;
    sljit_s32 src2; sljit_sw src2w;

    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &type))
	return EXCP_BADARG_N(env, 1, "not a valid simd_op2");
    if (!get_s32(env, argv[2], &dst_vreg))
	return EXCP_BADARG_N(env, 2, "not an integer");
    if (!get_s32(env, argv[3], &src1_vreg))
	return EXCP_BADARG_N(env, 3, "not an integer");
    if (!get_s32(env, argv[4], &src2))
	return EXCP_BADARG_N(env, 4, "not an integer");
    if (!get_sw(env, argv[5], &src2w))
	return EXCP_BADARG_N(env, 5, "not an integer");    
    ret = sljit_emit_simd_op2(cp->compiler, type, dst_vreg, src1_vreg,
			      src2, src2w);
    return nif_return(env, ret);
}


// create all tracing NIFs
#ifdef NIF_TRACE
static void trace_print_arg_list(ErlNifEnv* env,int argc,const ERL_NIF_TERM argv[])
{
    enif_fprintf(stdout, "(");
    if (argc > 0) {
	int i;
	if (enif_is_ref(env, argv[0])) {
	    // FIXME print object type if available
	    enif_fprintf(stdout, "%T", argv[0]);
	}
	else
	    enif_fprintf(stdout, "%T", argv[0]);
	for (i = 1; i < argc; i++)
	    enif_fprintf(stdout, ",%T", argv[i]);
    }
    enif_fprintf(stdout, ")");
}

#undef NIF
#define NIF(name, arity, func) \
static ERL_NIF_TERM trace##_##func##_##arity(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]) \
{ \
    ERL_NIF_TERM result;					\
    enif_fprintf(stdout, "ENTER %s", (name));			\
    trace_print_arg_list(env, argc, argv);			\
    enif_fprintf(stdout, "\r\n");				\
    result = func(env, argc, argv);				\
    enif_fprintf(stdout, "  RESULT=%T\r\n", (result));		\
    enif_fprintf(stdout, "LEAVE %s\r\n", (name));		\
    return result;						\
}

NIF_LIST
#endif

static void compiler_dtor(ErlNifEnv* env, void* ptr)
{
    UNUSED(env);
    DBG("compiler_dtor\r\n");
    sljit_free_compiler(((compiler_t*)ptr)->compiler);
}

static void code_dtor(ErlNifEnv* env, void* ptr)
{
    code_t* p = (code_t*) ptr;
    UNUSED(env);
    DBG("code_dtor\r\n");
    sljit_free_code(p->addr, p->exec_allocator_data);
}

// label is not used any more, but is reachamble through compuler struct
static void label_dtor(ErlNifEnv* env, void* ptr)
{
    UNUSED(env);
    UNUSED(ptr);    
    DBG("label_dtor\r\n");
}

// jump is not used any more, but is reachamble through compuler struct
static void jump_dtor(ErlNifEnv* env, void* ptr)
{
    UNUSED(env);
    UNUSED(ptr);    
    DBG("jump_dtor\r\n");
}

static void load_atoms(ErlNifEnv* env)
{
    LOAD_ATOM(sljit);
    LOAD_ATOM(ok);
    LOAD_ATOM(error);
    LOAD_ATOM(enoent);
    LOAD_ATOM(ealready);
    LOAD_ATOM(notsup);
    LOAD_ATOM(badarg);
    LOAD_ATOM(not_implemented);    
    LOAD_ATOM(compiled);
    LOAD_ATOM(alloc_failed);
    LOAD_ATOM(ex_alloc_failed);
    LOAD_ATOM(unsupported);
    LOAD_ATOM(bad_argument);
    LOAD_ATOM(undefined);

    LOAD_ATOM(code);
    LOAD_ATOM(code_size);
    LOAD_ATOM(exec_offset);
    LOAD_ATOM(return_type);
    LOAD_ATOM(argc);
    LOAD_ATOM(arg_type);

    LOAD_ATOM(void);
    LOAD_ATOM(word);
    LOAD_ATOM(word32);
    LOAD_ATOM(ptr);
    LOAD_ATOM(f64);
    LOAD_ATOM(f32);
}

static int sljit_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    UNUSED(load_info);
    ErlNifResourceFlags tried;
    nif_ctx_t* ctx;

    compiler_res = enif_open_resource_type(env, 0,
					   "sljit_compiler",
					   compiler_dtor,
					   ERL_NIF_RT_CREATE, &tried);
    label_res = enif_open_resource_type(env, 0,
					"sljit_label",
					label_dtor,
					ERL_NIF_RT_CREATE, &tried);
    jump_res = enif_open_resource_type(env, 0,
				       "sljit_jump",
				       jump_dtor,
				       ERL_NIF_RT_CREATE, &tried);
    code_res = enif_open_resource_type(env, 0,
				       "sljit_code",
				       code_dtor,
				       ERL_NIF_RT_CREATE,
				       &tried);        
    if ((ctx = (nif_ctx_t*) enif_alloc(sizeof(nif_ctx_t))) == NULL)
	return -1;
    ctx->first = NULL;
    
    load_atoms(env);
    *priv_data = ctx;
    return 0;
}

static int sljit_upgrade(ErlNifEnv* env, void** priv_data,
			 void** old_priv_data,
			 ERL_NIF_TERM load_info)
{
    UNUSED(env);
    UNUSED(load_info);
    ErlNifResourceFlags tried;

    compiler_res = enif_open_resource_type(env, 0,
					   "sljit_compiler",
					   compiler_dtor,
					   ERL_NIF_RT_CREATE |
					   ERL_NIF_RT_TAKEOVER,
					   &tried);
    label_res = enif_open_resource_type(env, 0,
					"sljit_label",
					label_dtor,
					ERL_NIF_RT_CREATE |
					ERL_NIF_RT_TAKEOVER,
					&tried);		       		
    jump_res = enif_open_resource_type(env, 0,
				       "sljit_jump",
				       jump_dtor,
				       ERL_NIF_RT_CREATE |
				       ERL_NIF_RT_TAKEOVER,
				       &tried);
    code_res = enif_open_resource_type(env, 0,
				       "sljit_code",
				       code_dtor,
				       ERL_NIF_RT_CREATE |
				       ERL_NIF_RT_TAKEOVER,
				       &tried);    

    load_atoms(env);

    *priv_data = *old_priv_data;
    return 0;
}

// Dirty optional since 2.7 and mandatory since 2.12
#if (ERL_NIF_MAJOR_VERSION > 2) || ((ERL_NIF_MAJOR_VERSION == 2) && (ERL_NIF_MINOR_VERSION >= 7))
#ifdef USE_DIRTY_SCHEDULER
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#else
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr),(0)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#endif
#else
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr)}
#endif

#undef NIF
#ifdef NIF_TRACE
#define NIF(name,arity,func) NIF_FUNC(name, arity, trace##_##func##_##arity),
#else
#define NIF(name,arity,func) NIF_FUNC(name, arity, func),
#endif


ErlNifFunc sljit_funcs[] =
{
    NIF_LIST
};



static void sljit_unload(ErlNifEnv* env, void* priv_data)
{
	UNUSED(env);
	UNUSED(priv_data);
}


ERL_NIF_INIT(sljit, sljit_funcs,
	     sljit_load, NULL,
	     sljit_upgrade, sljit_unload)
