// sljit api

#include <stdio.h>
#include <stdint.h>
#include <memory.h>
#include <math.h>
#include <unistd.h>

#include "erl_nif.h"
#include "sljitLir.h"

#define NIF_TRACE
#define DEBUG

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
    NIF("module",   2, nif_module) \
    NIF("function",   2, nif_function) \
    NIF("constant",   3, nif_constant) \
    NIF("get_platform_name", 0, nif_get_platform_name) \
    NIF("generate_code", 1, nif_generate_code) \
    NIF("unregister_code", 1, nif_unregister_code) \
    NIF("code_info",  2, nif_code_info)	 \
    NIF("call", 1, nif_avcall) \
    NIF("call", 2, nif_avcall) \
    NIF("call", 3, nif_avcall) \
    NIF("call", 4, nif_avcall) \
    NIF("call", 5, nif_avcall) \
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
    NIF("emit_simd_op2", 6, nif_emit_simd_op2) \
    NIF("get_label_addr", 1, nif_get_label_addr) \
    NIF("emit_const", 4, nif_emit_const) \
    NIF("set_constant", 3, nif_set_constant) \
    

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
DECL_ATOM(term);
// builtin functions (module sljit)
DECL_ATOM(print_sw);
DECL_ATOM(print_s32);
DECL_ATOM(print_uw);
DECL_ATOM(print_u32);
DECL_ATOM(print_f32);
DECL_ATOM(print_f64);
DECL_ATOM(print_ln);
DECL_ATOM(print_char);
DECL_ATOM(print_string);
DECL_ATOM(print_term);

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
static ErlNifResourceType* const_res;

typedef struct {
    struct sljit_jump* jump;
} jump_t;

typedef struct {
    struct sljit_label* label;
} label_t;

typedef struct {
    struct sljit_const* constp;
} const_t;


typedef struct code_t {
    void* addr;                   // main code area
    void* exec_allocator_data;
    sljit_sw exec_offset;
    sljit_uw code_size;
    struct module_entry_t* mod_ent;
    struct export_link_t* exp_list;  // list of links to export entries
    struct constant_entry_t* const_list; // list of generated constants
} code_t;

// signature type void only for return value
typedef enum {
    TYPE_VOID     = SLJIT_ARG_TYPE_RET_VOID, // 0
    TYPE_WORD     = SLJIT_ARG_TYPE_W,        // 1
    TYPE_WORD_R   = SLJIT_ARG_TYPE_W_R,      // 8+1    
    TYPE_WORD32   = SLJIT_ARG_TYPE_32,       // 2
    TYPE_WORD32_R = SLJIT_ARG_TYPE_32_R,     // 8+2    
    TYPE_PTR      = SLJIT_ARG_TYPE_P,        // 3
    TYPE_PTR_R    = SLJIT_ARG_TYPE_P_R,      // 8+3    
    TYPE_F64      = SLJIT_ARG_TYPE_F64,      // 4
    TYPE_F32      = SLJIT_ARG_TYPE_F32,      // 5
    TYPE_TERM     = 6,                               // 6
    TYPE_TERM_R   = (6 | SLJIT_ARG_TYPE_SCRATCH_REG) // 8+6
} sig_type_t;

#define ARGTYPE_RET(type)  ((type) & 0xf)
#define ARGTYPE(type,i)    (((type) >> ((i)*SLJIT_ARG_SHIFT)) & 0xf)
// 0xttttt = 4
// 0x0tttt = 3
// 0x00ttt = 2
// 0x000tt = 1
// 0x0000t = 0
#define ARGTYPE_ARGC(type)

// export entry jump and calls are generate from &ent->addr !
typedef struct export_entry_t {
    volatile void* addr; // address of the function (must be top of structure)
    ERL_NIF_TERM mod;    // the "module" part of the function name    
    ERL_NIF_TERM fun;    // the "function" part of the function name
    sljit_s32 arg_types;
    struct export_entry_t* next;  // next export entry (all entries)
} export_entry_t;

typedef struct export_link_t
{
    export_entry_t* exp;
    struct sljit_label* label;    // function label (set by function)
    struct export_link_t* next;
} export_link_t;

// export entry jump and calls are generate from &ent->addr !
typedef struct constant_entry_t {
    sljit_uw addr;       // address of the constant
    ERL_NIF_TERM mod;    // the "module" part of the constant name
    ERL_NIF_TERM name;   // the "constant" name
    const_t* ccp;        // resource pointer to sljit
    struct constant_entry_t* next;  // next export entry (all entries in module)
} constant_entry_t;

typedef struct module_entry_t {
    ERL_NIF_TERM mod;             // the "module" part of the function name
    code_t* current;              // resource pointer (kept)
    code_t* old;                  // resource pointer (kept)
    // constant_entry_t* const_list; // list of constants
    struct module_entry_t* next;  // next module entry
} module_entry_t;

typedef struct {
    ERL_NIF_TERM mod;    
    struct sljit_compiler* compiler;
    export_entry_t* xent;         // current export entry
    module_entry_t* ment;         // current module entry
    export_link_t*  exp_list;     // list of compiled functions
    constant_entry_t* const_list; // constant while compiling
} compiler_t;

typedef uintptr_t word_t;
typedef uint32_t  word32_t;
typedef uintptr_t ptr_t;
typedef double    float64_t;
typedef float     float32_t;

typedef struct {
    module_entry_t* mod_list;   // modules loaded
    export_entry_t* exp_list;   // exports loaded
} nif_ctx_t;

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

// how do we pass caller info here? thread local data?
static sljit_sw address_exception()
{
    enif_fprintf(stderr, "exception, code not loaded\n");
    return 0;
}

static nif_ctx_t* get_ctx(ErlNifEnv* env)
{
    return (nif_ctx_t*) enif_priv_data(env);
}

// allocate new module entry (fixme HASH) insert into nif_contxt
static module_entry_t* create_module(nif_ctx_t* ctx, ERL_NIF_TERM mod)
{
    module_entry_t* ment;
    ment = enif_alloc(sizeof(module_entry_t));
    ment->mod = mod;
    ment->current  = NULL;
    ment->old = NULL;
    // link into ctx module list
    ment->next = ctx->mod_list;
    ctx->mod_list = ment;
    return ment;
}

// allocate new export entry (fixme HASH) insert into nif context
export_entry_t* create_export(nif_ctx_t* ctx, ERL_NIF_TERM mod, ERL_NIF_TERM fun)
{
    export_entry_t* xent;
    xent = enif_alloc(sizeof(export_entry_t));
    xent->addr = address_exception;
    xent->mod = mod;
    xent->fun = fun;
    xent->arg_types = 0;
    xent->next = ctx->exp_list;
    ctx->exp_list = xent;
    return xent;
}

export_link_t* create_export_link(export_entry_t* exp)
{
    export_link_t* xlink;
    
    xlink = enif_alloc(sizeof(export_link_t));
    xlink->exp = exp;
    xlink->next = NULL;
    return xlink;
}

static module_entry_t* lookup_module(ErlNifEnv* env, ERL_NIF_TERM mod)
{
    nif_ctx_t* ctx = (nif_ctx_t*) enif_priv_data(env);
    module_entry_t* ment = ctx->mod_list;
    while(ment != NULL) {
	if (ment->mod == mod)
	    return ment;
	ment = ment->next;
    }
    return NULL;
}

static export_entry_t* lookup_export(nif_ctx_t* ctx, ERL_NIF_TERM mod, ERL_NIF_TERM fun)
{
    export_entry_t* xent = ctx->exp_list;
    while(xent != NULL) {
	if ((xent->mod == mod) && (xent->fun == fun))
	    return xent;
	xent = xent->next;
    }
    return NULL;    
}

// allocate new module entry (fixme HASH) insert into nif_contxt
static constant_entry_t* create_constant(ERL_NIF_TERM mod, ERL_NIF_TERM name)
{
    constant_entry_t* cent;
    cent = enif_alloc(sizeof(constant_entry_t));
    cent->addr = 0;
    cent->mod = mod;
    cent->name = name;
    cent->next = NULL;
    return cent;
}

static constant_entry_t* lookup_constant(compiler_t* cp, ERL_NIF_TERM name)
{
    constant_entry_t* cent = cp->const_list;
    while(cent != NULL) {
	if (cent->name == name)
	    return cent;
	cent = cent->next;
    }
    return NULL;
}

static int get_module(ErlNifEnv* env, ERL_NIF_TERM term, module_entry_t** mentp)
{
    code_t* crp = NULL;
    
    if (enif_is_atom(env, term)) {
	module_entry_t* ment;
	if ((ment = lookup_module(env, term)) == NULL)
	    return 0;
	*mentp = ment;
	return 1;
    }
    if (enif_get_resource(env, term, code_res, (void**)&crp)) {
	if (crp->mod_ent == NULL)
	    return 0;
	*mentp = crp->mod_ent;
	return 1;
    }
    return 0;
}

// load an export entry {mod,fun} or {code,fun}
static int get_export(ErlNifEnv* env, ERL_NIF_TERM term,
		      export_entry_t** xentp, code_t** crpp)
{
    export_entry_t* xent;
    const ERL_NIF_TERM* elems;

    int arity;
    code_t* crp;
    
    if (!enif_get_tuple(env, term, &arity, &elems) || (arity != 2))
	return 0;
    if (enif_is_atom(env, elems[0]) && enif_is_atom(env, elems[1])) {
	module_entry_t* ment;
	if ((ment = lookup_module(env, elems[0])) == NULL)
	    return 0;
	if ((xent = lookup_export(get_ctx(env), elems[0], elems[1])) == NULL)
	    return 0;
	if (xentp) *xentp = xent;
	if (crpp) *crpp = ment->current;
	return 1;
    }
    else if (enif_get_resource(env, elems[0], code_res, (void**)&crp) &&
	     enif_is_atom(env, elems[1])) {
	module_entry_t* ment;
	if ((ment = crp->mod_ent) == NULL)
	    return 0;
	if ((xent = lookup_export(get_ctx(env), ment->mod, elems[1])) == NULL)
	    return 0;
	if (xentp) *xentp = xent;
	if (crpp) *crpp = crp;
	return 1;
    }
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
    cp->mod = ATOM(undefined);
    cp->ment = NULL;  // set by nif_module()
    cp->xent = NULL;  // set by nif_function()
    cp->exp_list = NULL;
    cp->const_list = NULL;     
    sljit_compiler_verbose(cptr, stdout);
    term = enif_make_resource(env,cp);
    enif_release_resource(cp);
    return term;
}

// Set current module
ERL_NIF_TERM nif_module(ErlNifEnv* env, int argc,
			const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    module_entry_t* ment;
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!enif_is_atom(env, argv[1]))
	return EXCP_BADARG_N(env, 1, "not a module name");
    
    if (cp->mod != ATOM(undefined)) // module call twice!
	return enif_make_tuple2(env, ATOM(error), ATOM(ealready));
    if ((ment = lookup_module(env, argv[1])) == NULL)
	ment = create_module(get_ctx(env), argv[1]);
    cp->mod = argv[1];
    cp->ment = ment;   // current module
    return ATOM(ok);	
}

// Set current function and create an export entry
ERL_NIF_TERM nif_function(ErlNifEnv* env, int argc,
			  const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    module_entry_t* ment;
    export_entry_t* xent;
    export_link_t* xlink;
    struct sljit_label* label;
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!enif_is_atom(env, argv[1]))
	return EXCP_BADARG_N(env, 1, "not a function name");
    if ((ment = cp->ment) == NULL) // this is the module entry in compiler
    	return EXCP_BADARG_N(env, 0, "no module");

    if ((xent = lookup_export(get_ctx(env), ment->mod, argv[1])) == NULL)
	xent = create_export(get_ctx(env), ment->mod, argv[1]);

    xlink = create_export_link(xent);
    xlink->next = cp->exp_list;  // link into compilers export list
    cp->exp_list = xlink;

    if ((label = sljit_emit_label(cp->compiler)) == NULL)
	return nif_return(env, SLJIT_ERR_ALLOC_FAILED);
    xlink->label = label;
    cp->xent = xent;        // current function
    return ATOM(ok);
}

// Set current constant and create an export entry
ERL_NIF_TERM nif_constant(ErlNifEnv* env, int argc,
			  const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    const_t* ccp;    
    module_entry_t* ment;
    constant_entry_t* cent;
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!enif_is_atom(env, argv[1]))
	return EXCP_BADARG_N(env, 1, "not a constant name");
    if (!enif_get_resource(env, argv[2], const_res, (void**)&ccp))
	return EXCP_BADARG_N(env, 2, "not a constant");        

    if ((ment = cp->ment) == NULL) // this is the module entry in compiler
    	return EXCP_BADARG_N(env, 0, "no module");

    if (lookup_constant(cp, argv[1]) != NULL)
	return EXCP_BADARG_N(env, 1, "constant already created");
    cent = create_constant(ment->mod, argv[1]);
    cent->next = cp->const_list;
    cent->ccp = ccp;
    enif_keep_resource(ccp);
    cp->const_list = cent;
    return ATOM(ok);
}


ERL_NIF_TERM nif_get_platform_name(ErlNifEnv* env, int argc,
				   const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    UNUSED(argv);
    const char* name = sljit_get_platform_name();
    return enif_make_string(env, name, ERL_NIF_LATIN1);
}

static int arg_types_argc(sljit_s32 arg_types)
{
    if (arg_types & 0x70000) return 4;
    if (arg_types & 0x07000) return 3;
    if (arg_types & 0x00700) return 2;
    if (arg_types & 0x00070) return 1;
    return 0;
}

// translate TYPE_TERM = TYPE_WORD
//           TYPE_TERM_R = TYPE_WORD_R

static sljit_s32 arg_types_native(sljit_s32 arg_types)
{
    sljit_s32 arg_types1 = 0;
    int i;

    for (i = 16; i >= 0; i -= 4) {
	sljit_s32 part = (arg_types>>i) & 0xf;
	arg_types1 <<= 4;
	if ((part & 0x7) == TYPE_TERM)
	    arg_types1 |= (TYPE_WORD | (part & 0x8));
	else
	    arg_types1 |= part;
    }
    DBG("types_native: arg_types=%x, arg_types1=%x\r\n", arg_types, arg_types1);
    return arg_types1;
}


ERL_NIF_TERM nif_generate_code(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    void* code;
    ERL_NIF_TERM term;
    code_t* crp;
    module_entry_t* ment;
    ERL_NIF_TERM export_list;
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    code = sljit_generate_code(cp->compiler, 0, NULL);
    crp = enif_alloc_resource(code_res, sizeof(code_t));
    
    crp->addr = code;
    crp->exec_allocator_data = NULL;
    crp->exec_offset = sljit_get_executable_offset(cp->compiler);
    crp->code_size   = sljit_get_generated_code_size(cp->compiler);
    crp->mod_ent     = NULL;
    crp->exp_list    = NULL;
    crp->const_list  = NULL;
    
    export_list = enif_make_list(env, 0);    
    // are we generating a module?
    if ((ment = cp->ment) != NULL) {
	export_link_t* xlink;
	constant_entry_t* cent;

	crp->const_list = cp->const_list;
	crp->exp_list = cp->exp_list;
	crp->mod_ent = ment;

	cp->const_list = NULL;	
	cp->exp_list = NULL;
	cp->xent = NULL;
	cp->ment = NULL;
	cp->mod = ATOM(undefined);

	// FIXME: release ment->old
	ment->old = ment->current;

	// now scan the export entries
	xlink = crp->exp_list;
	while(xlink != NULL) {
	    export_entry_t* xent = xlink->exp;
	    void* addr = (void*) sljit_get_label_addr(xlink->label);
	    __atomic_store_n(&xent->addr,addr,__ATOMIC_RELAXED);
	    xlink = xlink->next;
	}
	ment->current = crp;

	// now scan the constant entries
	cent = crp->const_list;
	while(cent != NULL) {
	    const_t* cnst = cent->ccp;
	    sljit_uw addr = sljit_get_const_addr(cnst->constp);
	    cent->addr = addr;
	    cent = cent->next;
	}

	// collect all {Mod,Fun} list
	xlink = crp->exp_list;
	while(xlink != NULL) {
	    export_entry_t* xent = xlink->exp;
	    int nargs = arg_types_argc(xent->arg_types);
	    ERL_NIF_TERM mfa = enif_make_tuple3(env, xent->mod, xent->fun,
						enif_make_int(env, nargs));
	    export_list = enif_make_list_cell(env, mfa, export_list);
	    xlink = xlink->next;
	}
	enif_keep_resource(crp);
    }
    term = enif_make_resource(env,crp);
    enif_release_resource(crp);
    return enif_make_tuple2(env, export_list, term);
}

// purge code?
ERL_NIF_TERM nif_unregister_code(ErlNifEnv* env, int argc,
				 const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    module_entry_t* ment;
    code_t* crp;
    
    if (!enif_is_atom(env, argv[0]))
	return EXCP_BADARG_N(env, 0, "not atom");
    if ((ment = lookup_module(env, argv[0])) == NULL)
	return enif_make_tuple2(env, ATOM(error), ATOM(enoent));

    crp = ment->old;
    ment->old = ment->current;
    ment->current = NULL;
    if (crp != NULL) {
	enif_release_resource(crp);
    }
    return ATOM(ok);    
}


ERL_NIF_TERM make_type(ErlNifEnv* env, sig_type_t type)
{
    UNUSED(env);
    switch(type) {
    case TYPE_VOID: return ATOM(void);
    case TYPE_WORD:
    case TYPE_WORD_R: return ATOM(word);
    case TYPE_WORD32:
    case TYPE_WORD32_R: return ATOM(word32);
    case TYPE_PTR:
    case TYPE_PTR_R: return ATOM(ptr);
    case TYPE_F64: return ATOM(f64);
    case TYPE_F32: return ATOM(f32);
    case TYPE_TERM:
    case TYPE_TERM_R: return ATOM(term);	
    default: return ATOM(undefined);
    }
}

ERL_NIF_TERM nif_code_info(ErlNifEnv* env, int argc,
			   const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    code_t* crp = NULL;
    export_entry_t* xent = NULL;
    module_entry_t* ment = NULL;
    
    if (get_module(env, argv[0], &ment))
	crp = ment->current;
    else if (enif_get_resource(env, argv[0], code_res, (void**)&crp))
	ment = crp->mod_ent;
    else if (get_export(env, argv[0], &xent, &crp)) {
	if (xent->addr == address_exception)
	    return EXCP_BADARG_N(env, 0, "not loaded");
    }

    if (argv[1] == ATOM(code)) {
	ERL_NIF_TERM bin;
	unsigned char* dst;
	if ((crp == NULL) || (crp->code_size == 0))
	    return ATOM(undefined);
	dst = enif_make_new_binary(env, crp->code_size, &bin);
	memcpy(dst, crp->addr, crp->code_size);
	return bin;
    }
    if (argv[1] == ATOM(code_size)) {
	if (crp == NULL)
	    return ATOM(undefined);	    
	return make_uw(env, crp->code_size);
    }
    if (argv[1] == ATOM(exec_offset)) {
	if (crp == NULL)
	    return ATOM(undefined);	    
	return make_sw(env, crp->exec_offset);
    }
    if (xent != NULL) {
	if (argv[1] == ATOM(argc)) {
	    int argc = arg_types_argc(xent->arg_types);
	    return enif_make_int(env, argc);
	}
	if (argv[1] == ATOM(return_type)) {
	    return make_type(env, ARGTYPE_RET(xent->arg_types));
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
	    if (index > arg_types_argc(xent->arg_types)) return ATOM(undefined);
	    return make_type(env, ARGTYPE(xent->arg_types,index));
	}
	return EXCP_BADARG_N(env, 1, "not a code info");
    }
    return EXCP_BADARG_N(env, 0, "not mod/fun pair");
}

#include <avcall.h>

typedef union code_val {
    sljit_sw  sw;
    sljit_s32 s32;
    void*     ptr;
    double    f64;
    float     f32;
    ErlNifBinary bin;
} code_val_t;


ERL_NIF_TERM nif_avcall(ErlNifEnv* env, int argc,
			const ERL_NIF_TERM argv[])
{
    code_val_t ret;
    code_val_t arg[4];
    export_entry_t* xent = NULL;
    code_t* crp = NULL;
    int i;
    av_alist alist;

    if (!get_export(env, argv[0], &xent, &crp))
	return EXCP_BADARG_N(env, 0, "not code");
    
    if (xent->addr == address_exception)
	return EXCP_BADARG_N(env, 0, "not loaded");
    if (arg_types_argc(xent->arg_types) != argc-1)
	return EXCP_BADARG_N(env, 0, "wrong number of arguments");
    
    switch (ARGTYPE_RET(xent->arg_types)) {
    case TYPE_VOID:
	av_start_void(alist, xent->addr);
    case TYPE_TERM:
    case TYPE_TERM_R:
    case TYPE_WORD:
    case TYPE_WORD_R:
	av_start_long(alist, xent->addr, (sljit_sw*) &ret.sw);
	break;
    case TYPE_WORD32:
    case TYPE_WORD32_R:	
	av_start_int(alist, xent->addr, (sljit_s32*) &ret.s32);
	break;
    case TYPE_F32:
	av_start_float(alist, xent->addr, (float*) &ret.f32);
	break;
    case TYPE_F64:
	av_start_double(alist, xent->addr, (double*) &ret.f64);
	break;
    case TYPE_PTR:
    case TYPE_PTR_R:	
	av_start_ptr(alist, xent->addr, void*,  &ret.ptr);
	break;
    }

    // parse all arguments
    for (i = 0; i < argc-1; i++) {
	switch(ARGTYPE(xent->arg_types, i+1)) {
	case TYPE_TERM:
	case TYPE_TERM_R:
	    arg[i].sw = (sljit_sw) argv[i+1];
	    av_long(alist, (long) arg[i].sw);
	    break;
	case TYPE_WORD:
	case TYPE_WORD_R:	    
	    if (!get_sw(env, argv[i+1], &arg[i].sw))
		return EXCP_BADARG_N(env, i+1, "not an integer");
	    av_long(alist, (long) arg[i].sw);
	    break;
	case TYPE_WORD32:
	case TYPE_WORD32_R:	    
	    if (!get_s32(env, argv[i+1], &arg[i].s32))
		return EXCP_BADARG_N(env, i+1, "not an integer");
	    av_int(alist, (int) arg[i].s32);
	    break;
	case TYPE_PTR:
	case TYPE_PTR_R:
	    if (!enif_inspect_iolist_as_binary(env, argv[i+1], &arg[i].bin))
		return EXCP_BADARG_N(env, i+1, "not an iolist");
	    av_ptr(alist, void*, arg[i].bin.data);
	    break;
	case TYPE_F64:
	    if (!enif_get_double(env, argv[i+1], &arg[i].f64))
		return EXCP_BADARG_N(env, i+1, "not a float");
	    av_double(alist, arg[i].f64);
	    break;
	case TYPE_F32:
	    if (!get_float(env, argv[i+1], &arg[i].f32))
		return EXCP_BADARG_N(env, i+1, "not a float");
	    av_float(alist, (float) arg[i].f32);	    
	    break;
	default:
	    return EXCP_BADARG_N(env, i+1, "internal error");
	}
    }

    av_call (alist);

    switch(ARGTYPE_RET(xent->arg_types)) {
    case TYPE_VOID:
	return ATOM(ok);
    case TYPE_TERM:
    case TYPE_TERM_R:
	return (ERL_NIF_TERM) ret.sw;
    case TYPE_WORD:
    case TYPE_WORD_R:
	return make_sw(env, ret.sw);
    case TYPE_WORD32:
    case TYPE_WORD32_R:	
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
    sljit_s32 arg_types1;    
    struct sljit_jump* jump;
    jump_t* jmp;
    ERL_NIF_TERM term;
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &type))
	return EXCP_BADARG_N(env, 1, "not an integer");
    if (!get_s32(env, argv[2], &arg_types))
	return EXCP_BADARG_N(env, 2, "not an integer");
    arg_types1 = arg_types_native(arg_types);
    if ((jump = sljit_emit_call(cp->compiler, type, arg_types1)) == NULL)
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

// Called after generate code!
ERL_NIF_TERM nif_get_label_addr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    label_t* lbl;

    if (!enif_get_resource(env, argv[0], label_res, (void**)&lbl))
	return EXCP_BADARG_N(env, 0, "not a label");
    // fixme check compiler for error?
    return make_uw(env, sljit_get_label_addr(lbl->label));
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
    export_entry_t* xent;    
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &type))
	return EXCP_BADARG_N(env, 1, "not an integer");
    if (!enif_is_atom(env, argv[2]))
	return EXCP_BADARG_N(env, 2, "not an atom");
    if (!enif_is_atom(env, argv[3]))
	return EXCP_BADARG_N(env, 3, "not an atom");

    if ((xent = lookup_export(get_ctx(env), argv[2], argv[3])) == NULL)
	xent = create_export(get_ctx(env), argv[2], argv[3]);
    
    ret = sljit_emit_ijump(cp->compiler, type, SLJIT_MEM0(), (sljit_sw) xent);
    return nif_return(env, ret);
}

ERL_NIF_TERM nif_emit_icall(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    sljit_s32 ret;
    sljit_s32 type;
    sljit_s32 arg_types;
    sljit_s32 arg_types1;
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
    arg_types1 = arg_types_native(arg_types);    
    ret = sljit_emit_icall(cp->compiler, type, arg_types1, src, srcw);    
    return nif_return(env, ret);
}

ERL_NIF_TERM nif_emit_mcall(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    sljit_s32 ret;
    sljit_s32 type;
    sljit_s32 arg_types;
    sljit_s32 arg_types1;    
    export_entry_t* xent;
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &type))
	return EXCP_BADARG_N(env, 1, "not an integer");
    if (!get_s32(env, argv[2], &arg_types))
	return EXCP_BADARG_N(env, 2, "not an integer");
    if (!enif_is_atom(env, argv[3]))
	return EXCP_BADARG_N(env, 3, "not an atom");
    if (!enif_is_atom(env, argv[4]))
	return EXCP_BADARG_N(env, 4, "not an atom");

    if ((xent = lookup_export(get_ctx(env), argv[3], argv[4])) == NULL) {
	xent = create_export(get_ctx(env), argv[3], argv[4]);
	xent->arg_types = arg_types;  // hint!
    }
    DBG("mcall: %T:%T type=%x, argtypes=%x, addr=%p",
	xent->mod, xent->fun, type, arg_types, (void*) xent);

    // FIXME: generate atomic load of addr
    arg_types1 = arg_types_native(arg_types);
    ret = sljit_emit_icall(cp->compiler, type, arg_types1,
			   SLJIT_MEM0(), (sljit_sw) &xent->addr);
    return nif_return(env, ret);
}

ERL_NIF_TERM nif_emit_enter(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    sljit_s32 options;
    sljit_s32 arg_types;
    sljit_s32 arg_types1;    
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
    arg_types1 = arg_types_native(arg_types);
    ret = sljit_emit_enter(cp->compiler, options, arg_types1, scratches,
			   saveds, local_size);
    if (cp->xent != NULL)
	cp->xent->arg_types = arg_types;
    return nif_return(env, ret);
}

ERL_NIF_TERM nif_set_context(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    sljit_s32 options;
    sljit_s32 arg_types;
    sljit_s32 arg_types1;    
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
    arg_types1 = arg_types_native(arg_types);    
    ret = sljit_set_context(cp->compiler, options, arg_types1, scratches,
			    saveds, local_size);
    if (cp->xent != NULL)
	cp->xent->arg_types = arg_types;    
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

ERL_NIF_TERM nif_emit_const(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    compiler_t* cp;
    const_t* cpp;
    sljit_s32 dst; sljit_sw dstw;
    sljit_sw init_value;
    ERL_NIF_TERM term;
    
    if (!enif_get_resource(env, argv[0], compiler_res, (void**)&cp))
	return EXCP_BADARG_N(env, 0, "not a compiler");
    if (!get_s32(env, argv[1], &dst))
	return EXCP_BADARG_N(env, 1, "not an integer");
    if (!get_sw(env, argv[2], &dstw))
	return EXCP_BADARG_N(env, 2, "not an integer");
    if (!get_sw(env, argv[3], &init_value))
	return EXCP_BADARG_N(env, 3, "not an integer");

    cpp = enif_alloc_resource(const_res, sizeof(const_t));    
    cpp->constp = sljit_emit_const(cp->compiler, dst, dstw, init_value);
    term = enif_make_resource(env,cpp);
    enif_release_resource(cpp);
    return term;
}

ERL_NIF_TERM nif_get_const_addr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    const_t* ccp;
    sljit_uw ret;
    
    if (!enif_get_resource(env, argv[0], const_res, (void**)&ccp))
	return EXCP_BADARG_N(env, 0, "not a constant");
    ret = sljit_get_const_addr(ccp->constp);
    return make_uw(env, ret);
}

// set_constant(Module/Code, Name, Value) 
ERL_NIF_TERM nif_set_constant(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[])
{
    UNUSED(argc);
    code_t* crp = NULL;    
    sljit_sw new_constant;
    constant_entry_t* cent;
    module_entry_t* ment = NULL;    

    if (get_module(env, argv[0], &ment))
	crp = ment->current;
    else if (enif_get_resource(env, argv[0], code_res, (void**)&crp))
	ment = crp->mod_ent;
    else 
	return EXCP_BADARG_N(env, 0, "not code");    
    if (!enif_is_atom(env, argv[1]))
	return EXCP_BADARG_N(env, 1, "not a constant name");
    if (!get_sw(env, argv[2], &new_constant))
	return EXCP_BADARG_N(env, 2, "not an integer");
    cent = crp->const_list;
    while(cent != NULL) {
	if (cent->name == argv[1]) {
	    sljit_set_const(cent->addr, new_constant, crp->exec_offset);
	    return ATOM(ok);
	}
	cent = cent->next;
    }
    return EXCP_BADARG_N(env, 1, "not a constant name");    
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
    compiler_t* cp = (compiler_t*)ptr;
    export_link_t* xlink = cp->exp_list;    

    DBG("compiler_dtor\r\n");    

    while(xlink != NULL) {
	export_link_t* xnext = xlink->next;
	enif_free(xlink);
	xlink = xnext;
    }
    cp->xent = NULL;
    cp->ment = NULL;
    sljit_free_compiler(((compiler_t*)ptr)->compiler);
}

static void code_dtor(ErlNifEnv* env, void* ptr)
{
    code_t* crp = (code_t*) ptr;
    export_link_t* xlink = crp->exp_list;
    UNUSED(env);
    DBG("code_dtor\r\n");

    while(xlink != NULL) {
	export_link_t* xnext = xlink->next;
	export_entry_t* xent = xlink->exp;
	sljit_uw a = (sljit_uw) xent->addr;
	
	if ( (a >= ((sljit_uw) crp->addr)) &&
	     (a <= ((sljit_uw) crp->addr)+crp->code_size)) {
	    // clear address only if address pointed to is beeing freed
	    __atomic_store_n(&xent->addr,address_exception,__ATOMIC_RELAXED);
	}
	enif_free(xlink);
	xlink = xnext;
    }
    crp->mod_ent = NULL;
    crp->exp_list = NULL;
    // FIXME: scan p->mod_list and clean up all export_entries
    sljit_free_code(crp->addr, crp->exec_allocator_data);
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

// const is not used any more, but is reachamble through compuler struct
static void const_dtor(ErlNifEnv* env, void* ptr)
{
    UNUSED(env);
    UNUSED(ptr);    
    DBG("const_dtor\r\n");
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
    LOAD_ATOM(term);
    
    LOAD_ATOM(print_sw);
    LOAD_ATOM(print_s32);
    LOAD_ATOM(print_uw);
    LOAD_ATOM(print_u32);
    LOAD_ATOM(print_f32);
    LOAD_ATOM(print_f64);
    LOAD_ATOM(print_ln);
    LOAD_ATOM(print_char);
    LOAD_ATOM(print_string);
    LOAD_ATOM(print_term);
}

#define OUT stdout

static int print_sw(sljit_sw value)
{
    return enif_fprintf(OUT, "%ld", value);
}

static int print_s32(sljit_s32 value)
{
    return enif_fprintf(OUT, "%d", value);
}

static sljit_sw print_uw(sljit_sw value)
{
    return enif_fprintf(OUT, "%lu", value);
}

static sljit_sw print_u32(sljit_u32 value)
{
    return enif_fprintf(OUT, "%u", value);
}

static sljit_sw print_f32(float value)
{
    return enif_fprintf(OUT, "%f", value);
}

static sljit_sw print_f64(double value)
{
    return enif_fprintf(OUT, "%f", value);
}

static sljit_sw print_ln()
{
    return enif_fprintf(OUT, "\r\n");
}

static sljit_sw print_char(char c)
{
    return enif_fprintf(OUT, "%c", c);
}

static sljit_sw print_string(char* str)
{
    return enif_fprintf(OUT, "%s", str);
}

static sljit_sw print_term(ERL_NIF_TERM t)
{
    return enif_fprintf(OUT, "%T", t);
}

static void load_cfunc(nif_ctx_t* ctx, ERL_NIF_TERM mod, ERL_NIF_TERM fun,
		       void* addr, sljit_s32 arg_types)
{
    export_entry_t* xent;
    xent = create_export(ctx, mod, fun);
    xent->addr = addr;
    xent->arg_types = arg_types;
}
		       

static int load_built_ins(nif_ctx_t* ctx)
{
    module_entry_t* ment;

    if ((ment = create_module(ctx, ATOM(sljit))) == NULL)
	return -1;
    
    load_cfunc(ctx, ATOM(sljit), ATOM(print_sw), print_sw,
	       SLJIT_ARG_RETURN(TYPE_WORD_R) |
	       SLJIT_ARG_VALUE(TYPE_WORD_R, 1));
    load_cfunc(ctx, ATOM(sljit), ATOM(print_s32), print_s32,
	       SLJIT_ARG_RETURN(TYPE_WORD_R) |
	       SLJIT_ARG_VALUE(TYPE_WORD32_R, 1));
    load_cfunc(ctx, ATOM(sljit), ATOM(print_uw), print_uw,
	       SLJIT_ARG_RETURN(TYPE_WORD_R) |
	       SLJIT_ARG_VALUE(TYPE_WORD_R, 1));
    load_cfunc(ctx, ATOM(sljit), ATOM(print_u32), print_u32,
	       SLJIT_ARG_RETURN(TYPE_WORD_R) |
	       SLJIT_ARG_VALUE(TYPE_WORD_R, 1));
    load_cfunc(ctx, ATOM(sljit), ATOM(print_f32), print_f32,
	       SLJIT_ARG_RETURN(TYPE_WORD_R) |
	       SLJIT_ARG_VALUE(TYPE_F32, 1));
    load_cfunc(ctx, ATOM(sljit), ATOM(print_f64), print_f64,
	       SLJIT_ARG_RETURN(TYPE_WORD_R) |
	       SLJIT_ARG_VALUE(TYPE_F64, 1));
    load_cfunc(ctx, ATOM(sljit), ATOM(print_ln), print_ln,
	       SLJIT_ARG_RETURN(TYPE_WORD_R));
    load_cfunc(ctx, ATOM(sljit), ATOM(print_char), print_char,
	       SLJIT_ARG_RETURN(TYPE_WORD_R) |
	       SLJIT_ARG_VALUE(TYPE_WORD_R, 1));
    load_cfunc(ctx, ATOM(sljit), ATOM(print_string), print_string,
	       SLJIT_ARG_RETURN(TYPE_WORD_R) |
	       SLJIT_ARG_VALUE(TYPE_PTR_R, 1));
    load_cfunc(ctx, ATOM(sljit), ATOM(print_term), print_term,
	       SLJIT_ARG_RETURN(TYPE_WORD_R) |
	       SLJIT_ARG_VALUE(TYPE_TERM_R, 1));
    
    return 0;
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
    const_res = enif_open_resource_type(env, 0,
					"sljit_const",
					const_dtor,
					ERL_NIF_RT_CREATE, &tried);    
    code_res = enif_open_resource_type(env, 0,
				       "sljit_code",
				       code_dtor,
				       ERL_NIF_RT_CREATE,
				       &tried);
    if ((ctx = (nif_ctx_t*) enif_alloc(sizeof(nif_ctx_t))) == NULL)
	return -1;
    ctx->mod_list = NULL;
    ctx->exp_list = NULL;
    load_atoms(env);

    load_built_ins(ctx);
    
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
    const_res = enif_open_resource_type(env, 0,
					"sljit_const",
					const_dtor,
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

    // update built_ins ? 
    // load_built_ins(ctx); 

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
    nif_ctx_t* ctx = (nif_ctx_t*) priv_data;
    module_entry_t* ment;
    export_entry_t* xent;

    // delete all modules
    ment = ctx->mod_list;
    while(ment != NULL) {
	module_entry_t* mnext = ment->next;
	enif_free(ment);
	ment = mnext;
    }

    // delete all export entries
    xent = ctx->exp_list;
    while(xent != NULL) {
	export_entry_t* xnext = xent->next;
	enif_free(xent);
	xent = xnext;
    }
    enif_free(ctx);
}


ERL_NIF_INIT(sljit, sljit_funcs,
	     sljit_load, NULL,
	     sljit_upgrade, sljit_unload)
