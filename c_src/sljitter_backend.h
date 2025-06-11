#ifndef __SLJITTER_BACKEND_H__
#define __SLJITTER_BACKEND_H__

typedef enum {
    SLJITTER_ARCH_UNSUPPORTED = 0,
    SLJITTER_ARCH_X86_32 = 1,
    SLJITTER_ARCH_X86_64 = 2,
    SLJITTER_ARCH_ARM_V6 = 3,
    SLJITTER_ARCH_ARM_V7 = 4,
    SLJITTER_ARCH_ARM_THUMB2 = 5,
    SLJITTER_ARCH_ARM_64 = 6,
    SLJITTER_ARCH_PPC_32 = 7,
    SLJITTER_ARCH_PPC_64 = 8,
    SLJITTER_ARCH_MIPS_32 = 9,
    SLJITTER_ARCH_MIPS_64 = 10,
    SLJITTER_ARCH_RISCV_32 = 11,
    SLJITTER_ARCH_RISCV_64 = 12,
    SLJITTER_ARCH_S390X = 13,
    SLJITTER_ARCH_LOONGARCH_64 = 14,
    SLJITTER_ARCH_EMULATOR = 254,
    SLJITTER_ARCH_AUTO = 255,
} sljitter_architecture_t;

typedef union _sljitter_val_t {
    sljit_sw  sw;
    sljit_f64 f64;
} sljitter_val_t;

typedef struct _sljitter_backend_info_t {
    sljit_sw number_of_registers;
    sljit_sw number_of_scratch_registers;
    sljit_sw number_of_saved_registers;
    sljit_sw number_of_float_registers;
    sljit_sw number_of_saved_float_registers;
    sljit_sw number_of_vector_registers;
    sljit_sw number_of_saved_vector_registers;
    sljit_sw return_reg;
    sljit_sw sp_reg;
    sljit_sw vsize;
} sljitter_backend_info_t;

typedef struct _sljittter_backend_t {
    sljitter_architecture_t arch;
    sljitter_backend_info_t info;
    const char* (*get_platform_name)(void);
    sljit_s32 (*has_cpu_feature)(sljit_s32 feature_type);

    struct sljit_compiler* (*create_compiler)(void* allocator_data);
    void (*free_compiler)(struct sljit_compiler *compiler);    
    sljit_s32 (*emit_op0)(struct sljit_compiler *compiler, sljit_s32 op);
    sljit_s32 (*emit_op1)(struct sljit_compiler *compiler, sljit_s32 op,
			  sljit_s32 dst, sljit_sw dstw,
			  sljit_s32 src, sljit_sw srcw);
    sljit_s32 (*emit_op2)(struct sljit_compiler *compiler, sljit_s32 op,
			  sljit_s32 dst, sljit_sw dstw,
			  sljit_s32 src1, sljit_sw src1w,
			  sljit_s32 src2, sljit_sw src2w);
    sljit_s32 (*emit_op2u)(struct sljit_compiler *compiler, sljit_s32 op,
			   sljit_s32 src1, sljit_sw src1w,
			   sljit_s32 src2, sljit_sw src2w);
    sljit_s32 (*emit_op2r)(struct sljit_compiler *compiler, sljit_s32 op,
			   sljit_s32 dst_reg,
			   sljit_s32 src1, sljit_sw src1w,
			   sljit_s32 src2, sljit_sw src2w);
    sljit_s32 (*emit_shift_into)(struct sljit_compiler *compiler, sljit_s32 op,
				 sljit_s32 dst_reg,
				 sljit_s32 src1_reg,
				 sljit_s32 src2_reg,
				 sljit_s32 src3, sljit_sw src3w);
    sljit_s32 (*emit_op_src)(struct sljit_compiler *compiler, sljit_s32 op,
			     sljit_s32 src, sljit_sw srcw);
    sljit_s32 (*emit_op_dst)(struct sljit_compiler *compiler, sljit_s32 op,
			     sljit_s32 dst, sljit_sw dstw);

    sljit_s32 (*emit_fop1)(struct sljit_compiler *compiler, sljit_s32 op,
			   sljit_s32 dst, sljit_sw dstw,
			   sljit_s32 src, sljit_sw srcw);
    sljit_s32 (*emit_fop2)(struct sljit_compiler *compiler, sljit_s32 op,
			   sljit_s32 dst, sljit_sw dstw,
			   sljit_s32 src1, sljit_sw src1w,
			   sljit_s32 src2, sljit_sw src2w);
    sljit_s32 (*emit_fop2r)(struct sljit_compiler *compiler, sljit_s32 op,
			    sljit_s32 dst_freg,
			    sljit_s32 src1, sljit_sw src1w,
			    sljit_s32 src2, sljit_sw src2w);
    sljit_s32 (*emit_fset32)(struct sljit_compiler *compiler,
			     sljit_s32 freg, sljit_f32 value);
    sljit_s32 (*emit_fset64)(struct sljit_compiler *compiler,
			     sljit_s32 freg, sljit_f64 value);
    sljit_s32 (*emit_fcopy)(struct sljit_compiler *compiler, sljit_s32 op,
				  sljit_s32 freg, sljit_s32 reg);
    struct sljit_label* (*emit_label)(struct sljit_compiler *compiler);
    struct sljit_label* (*emit_aligned_label)(struct sljit_compiler *compiler,
					      sljit_s32 alignment,
					      struct sljit_read_only_buffer *buffers);
    struct sljit_jump* (*emit_jump)(struct sljit_compiler *compiler, sljit_s32 type);
    struct sljit_jump* (*emit_call)(struct sljit_compiler *compiler, sljit_s32 type, sljit_s32 arg_types);
    struct sljit_jump* (*emit_cmp)(struct sljit_compiler *compiler,
				   sljit_s32 type,
				   sljit_s32 src1, sljit_sw src1w,
				   sljit_s32 src2, sljit_sw src2w);
    struct sljit_jump* (*emit_fcmp)(struct sljit_compiler *compiler, sljit_s32 type,
				    sljit_s32 src1, sljit_sw src1w,
				    sljit_s32 src2, sljit_sw src2w);
    struct sljit_jump* (*emit_op2cmpz)(struct sljit_compiler *compiler, sljit_s32 op,
				       sljit_s32 dst, sljit_sw dstw,
				       sljit_s32 src1, sljit_sw src1w,
				       sljit_s32 src2, sljit_sw src2w);
    void (*set_label)(struct sljit_jump *jump, struct sljit_label* label);
    void (*set_target)(struct sljit_jump *jump, sljit_uw target);
    
    sljit_s32 (*emit_ijump)(struct sljit_compiler *compiler, sljit_s32 type, sljit_s32 src, sljit_sw srcw);
    sljit_s32 (*emit_icall)(struct sljit_compiler *compiler, sljit_s32 type, sljit_s32 arg_types, sljit_s32 src, sljit_sw srcw);
    sljit_s32 (*emit_op_flags)(struct sljit_compiler *compiler, sljit_s32 op,
			       sljit_s32 dst, sljit_sw dstw,
			       sljit_s32 type);
    sljit_s32 (*emit_select)(struct sljit_compiler *compiler, sljit_s32 type,
			     sljit_s32 dst_reg,
			     sljit_s32 src1, sljit_sw src1w,
			     sljit_s32 src2_reg);
    sljit_s32 (*emit_fselect)(struct sljit_compiler *compiler, sljit_s32 type,
			      sljit_s32 dst_freg,
			      sljit_s32 src1, sljit_sw src1w,
			      sljit_s32 src2_freg);
    sljit_s32 (*emit_mem)(struct sljit_compiler *compiler, sljit_s32 type,
			  sljit_s32 reg,
			  sljit_s32 mem, sljit_sw memw);
    sljit_s32 (*emit_mem_update)(struct sljit_compiler *compiler, sljit_s32 type,
				 sljit_s32 reg,
				 sljit_s32 mem, sljit_sw memw);
    sljit_s32 (*emit_fmem)(struct sljit_compiler *compiler, sljit_s32 type,
			   sljit_s32 freg,
			   sljit_s32 mem, sljit_sw memw);
    sljit_s32 (*emit_fmem_update)(struct sljit_compiler *compiler,
				  sljit_s32 type,
				  sljit_s32 freg,
				  sljit_s32 mem, sljit_sw memw);
    sljit_s32 (*emit_simd_mov)(struct sljit_compiler *compiler, sljit_s32 type,
			       sljit_s32 vreg,
			       sljit_s32 srcdst, sljit_sw srcdstw);

    sljit_s32 (*emit_enter)(struct sljit_compiler *compiler,
			    sljit_s32 options, sljit_s32 arg_types,
			    sljit_s32 scratches, sljit_s32 saveds, sljit_s32 local_size);

    sljit_s32 (*set_context)(struct sljit_compiler *compiler,
			     sljit_s32 options, sljit_s32 arg_types,
			     sljit_s32 scratches, sljit_s32 saveds, sljit_s32 local_size);

    sljit_s32 (*emit_return_void)(struct sljit_compiler *compiler);

    sljit_s32 (*emit_return)(struct sljit_compiler *compiler, sljit_s32 op,
				sljit_s32 src, sljit_sw srcw);

    sljit_s32 (*emit_return_to)(struct sljit_compiler *compiler,
				   sljit_s32 src, sljit_sw srcw);

    void* (*generate_code)(struct sljit_compiler *compiler, sljit_s32 options, void *exec_allocator_data);
    void (*free_code)(void* code, void *exec_allocator_data);

    sljit_s32 (*emit_simd_replicate)(struct sljit_compiler *compiler, sljit_s32 type,
				     sljit_s32 vreg,
				     sljit_s32 src, sljit_sw srcw);
    sljit_s32 (*emit_simd_lane_mov)(struct sljit_compiler *compiler, sljit_s32 type,
				    sljit_s32 vreg, sljit_s32 lane_index,
				    sljit_s32 srcdst, sljit_sw srcdstw);
    sljit_s32 (*emit_simd_lane_replicate)(struct sljit_compiler *compiler, sljit_s32 type,
	sljit_s32 vreg,
	sljit_s32 src, sljit_s32 src_lane_index);
    sljit_s32 (*emit_simd_extend)(struct sljit_compiler *compiler,
				     sljit_s32 type,
				     sljit_s32 vreg,
				     sljit_s32 src, sljit_sw srcw);
    sljit_s32 (*emit_simd_sign)(struct sljit_compiler *compiler,
				   sljit_s32 type,
				   sljit_s32 vreg,
				   sljit_s32 dst, sljit_sw dstw);
    sljit_s32 (*emit_simd_op2)(struct sljit_compiler *compiler,
			       sljit_s32 type,
			       sljit_s32 dst_vreg, sljit_s32 src1_vreg,
			       sljit_s32 src2, sljit_sw src2w);
    sljit_s32 (*emit_simd_arith_op2)(struct sljit_compiler *compiler,
				     sljit_s32 type,
				     sljit_s32 dst_vreg, sljit_s32 src1_vreg,
				     sljit_s32 src2, sljit_sw src2w);    
    sljit_s32 (*emit_atomic_load)(struct sljit_compiler *compiler,
				  sljit_s32 op,
				  sljit_s32 dst_reg,
				  sljit_s32 mem_reg);
    sljit_s32 (*emit_atomic_store)(struct sljit_compiler *compiler,
				   sljit_s32 op,
				   sljit_s32 src_reg,
				   sljit_s32 mem_reg,
				   sljit_s32 temp_reg);
    sljit_s32 (*get_local_base)(struct sljit_compiler *compiler,
				sljit_s32 dst, sljit_sw dstw,
				sljit_sw offset);
    struct sljit_const* (*emit_const)(struct sljit_compiler *compiler,
				      sljit_s32 op,
				      sljit_s32 dst, sljit_sw dstw,
				      sljit_sw init_value);
    struct sljit_jump* (*emit_op_addr)(struct sljit_compiler *compiler,
				       sljit_s32 op,
				       sljit_s32 dst, sljit_sw dstw);

    void (*set_jump_addr)(sljit_uw addr, sljit_uw new_target, sljit_sw executable_offset);
    void (*set_const)(sljit_uw addr, sljit_s32 op, sljit_sw new_constant, sljit_sw executable_offset);

    sljit_uw (*get_const_addr)(struct sljit_const *const_);
    sljit_uw (*get_label_addr)(struct sljit_label *label);
    sljit_uw (*get_jump_addr)(struct sljit_jump *jump);

    sljit_sw (*get_executable_offset)(struct sljit_compiler *compiler);
    sljit_uw (*get_generated_code_size)(struct sljit_compiler *compiler);

    // run (emulator)
    sljit_s32 (*run)(emulator_state_t* st,
		     void* code, size_t code_size, void* addr,
		     void* arg, sljit_s32 arg_types,
		     void* ret);
    
} sljitter_backend_t;

#endif
