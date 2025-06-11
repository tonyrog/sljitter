// define a backend by:
/* Architecture selection. */
/* #define SLJIT_CONFIG_X86_32 1 */
/* #define SLJIT_CONFIG_X86_64 1 */
/* #define SLJIT_CONFIG_ARM_V6 1 */
/* #define SLJIT_CONFIG_ARM_V7 1 */
/* #define SLJIT_CONFIG_ARM_THUMB2 1 */
/* #define SLJIT_CONFIG_ARM_64 1 */
/* #define SLJIT_CONFIG_PPC_32 1 */
/* #define SLJIT_CONFIG_PPC_64 1 */
/* #define SLJIT_CONFIG_MIPS_32 1 */
/* #define SLJIT_CONFIG_MIPS_64 1 */
/* #define SLJIT_CONFIG_RISCV_32 1 */
/* #define SLJIT_CONFIG_RISCV_64 1 */
/* #define SLJIT_CONFIG_S390X 1 */
/* #define SLJIT_CONFIG_LOONGARCH_64 */

/* #define SLJIT_CONFIG_EMULATOR 1 */
/* #define SLJIT_CONFIG_AUTO 1 */
/* #define SLJIT_CONFIG_UNSUPPORTED 1 */
//
#define SLJIT_CONFIG_STATIC 1

//#include "sljitLir.h"
#include "sljitLir.c"

#include "sljitter_backend.h"

#if (defined SLJIT_CONFIG_X86_32 && SLJIT_CONFIG_X86_32)
#define BE_NAME sljitter_x86_32
#define BE_ARCH SLJITTER_ARCH_X86_32
#define BE_RUN  NULL
#define BE_SIMD_ARITH sljit_emit_simd_arith_op2
#include "slljitNativeX86_simd.c"
#elif (defined SLJIT_CONFIG_X86_64 && SLJIT_CONFIG_X86_64)
#define BE_NAME sljitter_x86_64
#define BE_ARCH SLJITTER_ARCH_X86_64
#define BE_RUN  NULL
#define BE_SIMD_ARITH sljit_emit_simd_arith_op2
#include "sljitNativeX86_simd.c"
#elif (defined SLJIT_CONFIG_ARM_V6 && SLJIT_CONFIG_ARM_V6)
#define BE_NAME sljitter_arm_v6
#define BE_ARCH SLJITTER_ARCH_ARM_V6
#define BE_RUN  NULL
#define BE_SIMD_ARITH NULL
#elif (defined SLJIT_CONFIG_ARM_V7 && SLJIT_CONFIG_ARM_V7)
#define BE_NAME sljitter_arm_v7
#define BE_ARCH SLJITTER_ARCH_ARM_V7
#define BE_RUN  NULL
#define BE_SIMD_ARITH NULL
#elif (defined SLJIT_CONFIG_ARM_THUMB2 && SLJIT_CONFIG_ARM_THUMB2)
#define BE_NAME sljitter_arm_thumb2
#define BE_ARCH SLJITTER_ARCH_ARM_THUMB2
#define BE_RUN  NULL
#define BE_SIMD_ARITH NULL
#elif (defined SLJIT_CONFIG_ARM_64 && SLJIT_CONFIG_ARM_64)
#define BE_NAME sljitter_arm_64
#define BE_ARCH SLJITTER_ARCH_ARM_64
#define BE_RUN  NULL
#define BE_SIMD_ARITH NULL
#elif (defined SLJIT_CONFIG_PPC_32 && SLJIT_CONFIG_PPC_32)
#define BE_NAME sljitter_ppc_32
#define BE_ARCH SLJITTER_ARCH_PPC_32
#define BE_RUN  NULL
#define BE_SIMD_ARITH NULL
#elif (defined SLJIT_CONFIG_PPC_64 && SLJIT_CONFIG_PPC_64)
#define BE_NAME sljitter_ppc_64
#define BE_ARCH SLJITTER_ARCH_PPC_64
#define BE_RUN  NULL
#define BE_SIMD_ARITH NULL
#elif (defined SLJIT_CONFIG_MIPS_32 && SLJIT_CONFIG_MIPS_32)
#define BE_NAME sljitter_mips_32
#define BE_ARCH SLJITTER_ARCH_MIPS_32
#define BE_RUN  NULL
#define BE_SIMD_ARITH NULL
#elif (defined SLJIT_CONFIG_MIPS_64 && SLJIT_CONFIG_MIPS_64)
#define BE_NAME sljitter_mips_64
#define BE_ARCH SLJITTER_ARCH_MIPS_64
#define BE_RUN  NULL
#define BE_SIMD_ARITH NULL
#elif (defined SLJIT_CONFIG_RISCV_32 && SLJIT_CONFIG_RISCV_32)
#define BE_NAME sljitter_riscv_32
#define BE_ARCH SLJITTER_ARCH_RISCV_32
#define BE_RUN  NULL
#define BE_SIMD_ARITH NULL
#elif (defined SLJIT_CONFIG_RISCV_64 && SLJIT_CONFIG_RISCV_64)
#define BE_NAME sljitter_riscv_64
#define BE_ARCH SLJITTER_ARCH_RISCV_64
#define BE_RUN  NULL
#define BE_SIMD_ARITH NULL
#elif (defined SLJIT_CONFIG_S390X && SLJIT_CONFIG_S390X)
#define BE_NAME sljitter_s390x
#define BE_ARCH SLJITTER_ARCH_S390X
#define BE_RUN  NULL
#define BE_SIMD_ARITH NULL
#elif (defined SLJIT_CONFIG_LOONGARCH_64 && SLJIT_CONFIG_LOONGARCH_64)
#define BE_NAME sljitter_loongarch_64
#define BE_ARCH SLJITTER_ARCH_LOONGARCH_64
#define BE_RUN  NULL
#define BE_SIMD_ARITH NULL
#elif (defined SLJIT_CONFIG_EMULATOR && SLJIT_CONFIG_EMULATOR)
#define BE_NAME sljitter_emulator
#define BE_ARCH SLJITTER_ARCH_EMULATOR
#define BE_RUN  sljit_run
#define BE_SIMD_ARITH sljit_emit_simd_arith_op2
#else
#error "architecture not defined"
#endif

sljitter_backend_t BE_NAME =
{
    .arch = BE_ARCH,
    .info.number_of_registers = SLJIT_NUMBER_OF_REGISTERS,
    .info.number_of_scratch_registers = SLJIT_NUMBER_OF_SCRATCH_REGISTERS,
    .info.number_of_saved_registers = SLJIT_NUMBER_OF_SAVED_REGISTERS,
    .info.number_of_float_registers = SLJIT_NUMBER_OF_FLOAT_REGISTERS,
    .info.number_of_saved_float_registers = SLJIT_NUMBER_OF_SAVED_FLOAT_REGISTERS,
    .info.number_of_vector_registers = SLJIT_NUMBER_OF_VECTOR_REGISTERS,
    .info.number_of_saved_vector_registers = SLJIT_NUMBER_OF_SAVED_VECTOR_REGISTERS,
    .info.return_reg = SLJIT_R0,
    .info.sp_reg = SLJIT_SP,
    .info.vsize = VSIZE,
    
    .get_platform_name = sljit_get_platform_name,
    .has_cpu_feature = sljit_has_cpu_feature,
    
    .create_compiler = sljit_create_compiler,
    .free_compiler = sljit_free_compiler,
    .emit_op0 = sljit_emit_op0,
    .emit_op1 = sljit_emit_op1,
    .emit_op2 = sljit_emit_op2,
    .emit_op2u = sljit_emit_op2u,
    .emit_op2r = sljit_emit_op2r,
    .emit_shift_into = sljit_emit_shift_into,
    .emit_op_src = sljit_emit_op_src,
    .emit_op_dst = sljit_emit_op_dst,

    .emit_fop1 = sljit_emit_fop1,
    .emit_fop2 = sljit_emit_fop2,
    .emit_fop2r = sljit_emit_fop2r,
    .emit_fset32 = sljit_emit_fset32,
    .emit_fset64 = sljit_emit_fset64,
    .emit_fcopy = sljit_emit_fcopy,
    .emit_label = sljit_emit_label,
    .emit_aligned_label = sljit_emit_aligned_label,
    .emit_jump = sljit_emit_jump,
    .emit_call = sljit_emit_call,
    .emit_cmp = sljit_emit_cmp,
    .emit_fcmp = sljit_emit_fcmp,
    .emit_op2cmpz = sljit_emit_op2cmpz,
    .set_label = sljit_set_label,
    .set_target = sljit_set_target,
    .emit_ijump = sljit_emit_ijump,
    .emit_icall = sljit_emit_icall,
    .emit_op_flags = sljit_emit_op_flags,
    .emit_select = sljit_emit_select,
    .emit_fselect = sljit_emit_fselect,
    .emit_mem = sljit_emit_mem,
    .emit_mem_update = sljit_emit_mem_update,
    .emit_fmem = sljit_emit_fmem,
    .emit_fmem_update = sljit_emit_fmem_update,
    .emit_simd_mov = sljit_emit_simd_mov,
    .emit_enter = sljit_emit_enter,
    .set_context = sljit_set_context,
    .emit_return_void = sljit_emit_return_void,
    .emit_return = sljit_emit_return,
    .emit_return_to = sljit_emit_return_to,
    .generate_code = sljit_generate_code,
    .free_code = sljit_free_code,
    .emit_simd_replicate = sljit_emit_simd_replicate,
    .emit_simd_lane_mov = sljit_emit_simd_lane_mov,
    .emit_simd_lane_replicate = sljit_emit_simd_lane_replicate,
    .emit_simd_extend = sljit_emit_simd_extend,
    .emit_simd_sign = sljit_emit_simd_sign,
    .emit_simd_op2 = sljit_emit_simd_op2,
    .emit_simd_arith_op2 = BE_SIMD_ARITH,    
    .emit_atomic_load = sljit_emit_atomic_load,
    .emit_atomic_store = sljit_emit_atomic_store,
    .get_local_base = sljit_get_local_base,
    .emit_const = sljit_emit_const,
    .emit_op_addr = sljit_emit_op_addr,
    .set_jump_addr = sljit_set_jump_addr,
    .set_const = sljit_set_const,

    .get_const_addr = sljit_get_const_addr,
    .get_label_addr = sljit_get_label_addr,
    .get_jump_addr = sljit_get_jump_addr,

    .get_executable_offset = sljit_get_executable_offset,
    .get_generated_code_size = sljit_get_generated_code_size,
    .run = BE_RUN,
};
