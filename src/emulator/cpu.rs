use super::memory::Memory;

const OP_CODE_FUNCTION_TABLE: [fn(&mut Cpu); 256] = [
    Cpu::op_nop,         // 0x00 : NOP
    Cpu::op_ld_bc_u16,   // 0x01 : LD BC,d16
    Cpu::op_ld_bc_a,     // 0x02 : LD (BC),A
    Cpu::op_inc_bc,      // 0x03 : INC BC
    Cpu::op_inc_b,       // 0x04 : INC B
    Cpu::op_dec_b,       // 0x05 : DEC B
    Cpu::op_ld_b_d8,     // 0x06 : LD B,d8
    Cpu::op_rlca,        // 0x07 : RLCA
    Cpu::op_ld_u16_sp,   // 0x08 : LD (a16),SP
    Cpu::op_add_hl_bc,   // 0x09 : ADD HL,BC
    Cpu::op_ld_a_bc,     // 0x0A : LD A,(BC)
    Cpu::op_dec_bc,      // 0x0B : DEC BC
    Cpu::op_inc_c,       // 0x0C : INC C
    Cpu::op_dec_c,       // 0x0D : DEC C
    Cpu::op_ld_c_d8,     // 0x0E : LD C,d8
    Cpu::op_rrca,        // 0x0F : RRCA
    Cpu::op_placeholder, // 0x10 : STOP 0
    Cpu::op_ld_de_u16,   // 0x11 : LD DE,d16
    Cpu::op_ld_de_a,     // 0x12 : LD (DE),A
    Cpu::op_inc_de,      // 0x13 : INC DE
    Cpu::op_inc_d,       // 0x14 : INC D
    Cpu::op_dec_d,       // 0x15 : DEC D
    Cpu::op_ld_d_d8,     // 0x16 : LD D,d8
    Cpu::op_rla,         // 0x17 : RLA
    Cpu::op_jr_r8,       // 0x18 : JR r8
    Cpu::op_add_hl_de,   // 0x19 : ADD HL,DE
    Cpu::op_ld_a_de,     // 0x1A : LD A,(DE)
    Cpu::op_dec_de,      // 0x1B : DEC DE
    Cpu::op_inc_e,       // 0x1C : INC E
    Cpu::op_dec_e,       // 0x1D : DEC E
    Cpu::op_ld_e_d8,     // 0x1E : LD E,d8
    Cpu::op_rra,         // 0x1F : RRA
    Cpu::op_jp_nz_r8,    // 0x20 : JR NZ,r8
    Cpu::op_ld_hl_u16,   // 0x21 : LD HL,d16
    Cpu::op_ld_hl_inc_a, // 0x22 : LD (HL+),A
    Cpu::op_inc_hl,      // 0x23 : INC HL
    Cpu::op_inc_h,       // 0x24 : INC H
    Cpu::op_dec_h,       // 0x25 : DEC H
    Cpu::op_ld_h_d8,     // 0x26 : LD H,d8
    Cpu::op_daa,         // 0x27 : DAA
    Cpu::op_jp_z_r8,     // 0x28 : JR Z,r8
    Cpu::op_add_hl_hl,   // 0x29 : ADD HL,HL
    Cpu::op_ld_a_hl_inc, // 0x2A : LD A,(HL+)
    Cpu::op_dec_hl,      // 0x2B : DEC HL
    Cpu::op_inc_l,       // 0x2C : INC L
    Cpu::op_dec_l,       // 0x2D : DEC L
    Cpu::op_ld_l_d8,     // 0x2E : LD L,d8
    Cpu::op_cpl,         // 0x2F : CPL
    Cpu::op_jp_nc_r8,    // 0x30 : JR NC,r8
    Cpu::op_ld_sp_u16,   // 0x31 : LD SP,d16
    Cpu::op_ld_hl_dec_a, // 0x32 : LD (HL-),A
    Cpu::op_inc_sp,      // 0x33 : INC SP
    Cpu::op_inc_hl_ind,  // 0x34 : INC (HL)
    Cpu::op_dec_hl_ind,  // 0x35 : DEC (HL)
    Cpu::op_ld_hl_d8,    // 0x36 : LD (HL),d8
    Cpu::op_scf,         // 0x37 : SCF
    Cpu::op_jp_c_r8,     // 0x38 : JR C,r8
    Cpu::op_add_hl_sp,   // 0x39 : ADD HL,SP
    Cpu::op_ld_a_hl_dec, // 0x3A : LD A,(HL-)
    Cpu::op_dec_sp,      // 0x3B : DEC SP
    Cpu::op_inc_a,       // 0x3C : INC A
    Cpu::op_dec_a,       // 0x3D : DEC A
    Cpu::op_ld_a_d8,     // 0x3E : LD A,d8
    Cpu::op_ccf,         // 0x3F : CCF
    Cpu::op_ld_b_b,      // 0x40 : LD B,B
    Cpu::op_ld_b_c,      // 0x41 : LD B,C
    Cpu::op_ld_b_d,      // 0x42 : LD B,D
    Cpu::op_ld_b_e,      // 0x43 : LD B,E
    Cpu::op_ld_b_h,      // 0x44 : LD B,H
    Cpu::op_ld_b_l,      // 0x45 : LD B,L
    Cpu::op_ld_b_hl,     // 0x46 : LD B,(HL)
    Cpu::op_ld_b_a,      // 0x47 : LD B,A
    Cpu::op_ld_c_b,      // 0x48 : LD C,B
    Cpu::op_ld_c_c,      // 0x49 : LD C,C
    Cpu::op_ld_c_d,      // 0x4A : LD C,D
    Cpu::op_ld_c_e,      // 0x4B : LD C,E
    Cpu::op_ld_c_h,      // 0x4C : LD C,H
    Cpu::op_ld_c_l,      // 0x4D : LD C,L
    Cpu::op_ld_c_hl,     // 0x4E : LD C,(HL)
    Cpu::op_ld_c_a,      // 0x4F : LD C,A
    Cpu::op_ld_d_b,      // 0x50 : LD D,B
    Cpu::op_ld_d_c,      // 0x51 : LD D,C
    Cpu::op_ld_d_d,      // 0x52 : LD D,D
    Cpu::op_ld_d_e,      // 0x53 : LD D,E
    Cpu::op_ld_d_h,      // 0x54 : LD D,H
    Cpu::op_ld_d_l,      // 0x55 : LD D,L
    Cpu::op_ld_d_hl,     // 0x56 : LD D,(HL)
    Cpu::op_ld_d_a,      // 0x57 : LD D,A
    Cpu::op_ld_e_b,      // 0x58 : LD E,B
    Cpu::op_ld_e_c,      // 0x59 : LD E,C
    Cpu::op_ld_e_d,      // 0x5A : LD E,D
    Cpu::op_ld_e_e,      // 0x5B : LD E,E
    Cpu::op_ld_e_h,      // 0x5C : LD E,H
    Cpu::op_ld_e_l,      // 0x5D : LD E,L
    Cpu::op_ld_e_hl,     // 0x5E : LD E,(HL)
    Cpu::op_ld_e_a,      // 0x5F : LD E,A
    Cpu::op_ld_h_b,      // 0x60 : LD H,B
    Cpu::op_ld_h_c,      // 0x61 : LD H,C
    Cpu::op_ld_h_d,      // 0x62 : LD H,D
    Cpu::op_ld_h_e,      // 0x63 : LD H,E
    Cpu::op_ld_h_h,      // 0x64 : LD H,H
    Cpu::op_ld_h_l,      // 0x65 : LD H,L
    Cpu::op_ld_h_hl,     // 0x66 : LD H,(HL)
    Cpu::op_ld_h_a,      // 0x67 : LD H,A
    Cpu::op_ld_l_b,      // 0x68 : LD L,B
    Cpu::op_ld_l_c,      // 0x69 : LD L,C
    Cpu::op_ld_l_d,      // 0x6A : LD L,D
    Cpu::op_ld_l_e,      // 0x6B : LD L,E
    Cpu::op_ld_l_h,      // 0x6C : LD L,H
    Cpu::op_ld_l_l,      // 0x6D : LD L,L
    Cpu::op_ld_l_hl,     // 0x6E : LD L,(HL)
    Cpu::op_ld_l_a,      // 0x6F : LD L,A
    Cpu::op_ld_hl_b,     // 0x70 : LD (HL),B
    Cpu::op_ld_hl_c,     // 0x71 : LD (HL),C
    Cpu::op_ld_hl_d,     // 0x72 : LD (HL),D
    Cpu::op_ld_hl_e,     // 0x73 : LD (HL),E
    Cpu::op_ld_hl_h,     // 0x74 : LD (HL),H
    Cpu::op_ld_hl_l,     // 0x75 : LD (HL),L
    Cpu::op_halt,        // 0x76 : HALT
    Cpu::op_ld_hl_a,     // 0x77 : LD (HL),A
    Cpu::op_ld_a_b,      // 0x78 : LD A,B
    Cpu::op_ld_a_c,      // 0x79 : LD A,C
    Cpu::op_ld_a_d,      // 0x7A : LD A,D
    Cpu::op_ld_a_e,      // 0x7B : LD A,E
    Cpu::op_ld_a_h,      // 0x7C : LD A,H
    Cpu::op_ld_a_l,      // 0x7D : LD A,L
    Cpu::op_ld_a_hl,     // 0x7E : LD A,(HL)
    Cpu::op_ld_a_a,      // 0x7F : LD A,A
    Cpu::op_add_a_b,     // 0x80 : ADD A,B
    Cpu::op_add_a_c,     // 0x81 : ADD A,C
    Cpu::op_add_a_d,     // 0x82 : ADD A,D
    Cpu::op_add_a_e,     // 0x83 : ADD A,E
    Cpu::op_add_a_h,     // 0x84 : ADD A,H
    Cpu::op_add_a_l,     // 0x85 : ADD A,L
    Cpu::op_add_a_hl,    // 0x86 : ADD A,(HL)
    Cpu::op_add_a_a,     // 0x87 : ADD A,A
    Cpu::op_adc_a_b,     // 0x88 : ADC A,B
    Cpu::op_adc_a_c,     // 0x89 : ADC A,C
    Cpu::op_adc_a_d,     // 0x8A : ADC A,D
    Cpu::op_adc_a_e,     // 0x8B : ADC A,E
    Cpu::op_adc_a_h,     // 0x8C : ADC A,H
    Cpu::op_adc_a_l,     // 0x8D : ADC A,L
    Cpu::op_adc_a_hl,    // 0x8E : ADC A,(HL)
    Cpu::op_adc_a_a,     // 0x8F : ADC A,A
    Cpu::op_sub_a_b,     // 0x90 : SUB B
    Cpu::op_sub_a_c,     // 0x91 : SUB C
    Cpu::op_sub_a_d,     // 0x92 : SUB D
    Cpu::op_sub_a_e,     // 0x93 : SUB E
    Cpu::op_sub_a_h,     // 0x94 : SUB H
    Cpu::op_sub_a_l,     // 0x95 : SUB L
    Cpu::op_sub_a_hl,    // 0x96 : SUB (HL)
    Cpu::op_sub_a_a,     // 0x97 : SUB A
    Cpu::op_sbc_a_b,     // 0x98 : SBC A,B
    Cpu::op_sbc_a_c,     // 0x99 : SBC A,C
    Cpu::op_sbc_a_d,     // 0x9A : SBC A,D
    Cpu::op_sbc_a_e,     // 0x9B : SBC A,E
    Cpu::op_sbc_a_h,     // 0x9C : SBC A,H
    Cpu::op_sbc_a_l,     // 0x9D : SBC A,L
    Cpu::op_sbc_a_hl,    // 0x9E : SBC A,(HL)
    Cpu::op_sbc_a_a,     // 0x9F : SBC A,A
    Cpu::op_and_a_b,     // 0xA0 : AND B
    Cpu::op_and_a_c,     // 0xA1 : AND C
    Cpu::op_and_a_d,     // 0xA2 : AND D
    Cpu::op_and_a_e,     // 0xA3 : AND E
    Cpu::op_and_a_h,     // 0xA4 : AND H
    Cpu::op_and_a_l,     // 0xA5 : AND L
    Cpu::op_and_a_hl,    // 0xA6 : AND (HL)
    Cpu::op_and_a_a,     // 0xA7 : AND A
    Cpu::op_xor_a_b,     // 0xA8 : XOR B
    Cpu::op_xor_a_c,     // 0xA9 : XOR C
    Cpu::op_xor_a_d,     // 0xAA : XOR D
    Cpu::op_xor_a_e,     // 0xAB : XOR E
    Cpu::op_xor_a_h,     // 0xAC : XOR H
    Cpu::op_xor_a_l,     // 0xAD : XOR L
    Cpu::op_xor_a_hl,    // 0xAE : XOR (HL)
    Cpu::op_xor_a_a,     // 0xAF : XOR A
    Cpu::op_or_a_b,      // 0xB0 : OR B
    Cpu::op_or_a_c,      // 0xB1 : OR C
    Cpu::op_or_a_d,      // 0xB2 : OR D
    Cpu::op_or_a_e,      // 0xB3 : OR E
    Cpu::op_or_a_h,      // 0xB4 : OR H
    Cpu::op_or_a_l,      // 0xB5 : OR L
    Cpu::op_or_a_hl,     // 0xB6 : OR (HL)
    Cpu::op_or_a_a,      // 0xB7 : OR A
    Cpu::op_cp_a_b,      // 0xB8 : CP B
    Cpu::op_cp_a_c,      // 0xB9 : CP C
    Cpu::op_cp_a_d,      // 0xBA : CP D
    Cpu::op_cp_a_e,      // 0xBB : CP E
    Cpu::op_cp_a_h,      // 0xBC : CP H
    Cpu::op_cp_a_l,      // 0xBD : CP L
    Cpu::op_cp_a_hl,     // 0xBE : CP (HL)
    Cpu::op_cp_a_a,      // 0xBF : CP A
    Cpu::op_ret_nz,      // 0xC0 : RET NZ
    Cpu::op_pop_bc,      // 0xC1 : POP BC
    Cpu::op_jp_nz_a16,   // 0xC2 : JP NZ,a16
    Cpu::op_jp_a16,      // 0xC3 : JP a16
    Cpu::op_call_nz_a16, // 0xC4 : CALL NZ,a16
    Cpu::op_push_bc,     // 0xC5 : PUSH BC
    Cpu::op_add_a_u8,    // 0xC6 : ADD A,d8
    Cpu::op_rst_00h,     // 0xC7 : RST 00H
    Cpu::op_ret_z,       // 0xC8 : RET Z
    Cpu::op_ret,         // 0xC9 : RET
    Cpu::op_jp_z_a16,    // 0xCA : JP Z,a16
    Cpu::op_prefix_cb,   // 0xCB : PREFIX CB
    Cpu::op_call_z_a16,  // 0xCC : CALL Z,a16
    Cpu::op_call_a16,    // 0xCD : CALL a16
    Cpu::op_adc_a_u8,    // 0xCE : ADC A,d8
    Cpu::op_rst_08h,     // 0xCF : RST 08H
    Cpu::op_ret_nc,      // 0xD0 : RET NC
    Cpu::op_pop_de,      // 0xD1 : POP DE
    Cpu::op_jp_nc_a16,   // 0xD2 : JP NC,a16
    Cpu::op_nop,         // 0xD3 : undefined
    Cpu::op_call_nc_a16, // 0xD4 : CALL NC,a16
    Cpu::op_push_de,     // 0xD5 : PUSH DE
    Cpu::op_sub_a_u8,    // 0xD6 : SUB d8
    Cpu::op_rst_10h,     // 0xD7 : RST 10H
    Cpu::op_ret_c,       // 0xD8 : RET C
    Cpu::op_reti,        // 0xD9 : RETI
    Cpu::op_jp_c_a16,    // 0xDA : JP C,a16
    Cpu::op_nop,         // 0xDB : undefined
    Cpu::op_call_c_a16,  // 0xDC : CALL C,a16
    Cpu::op_nop,         // 0xDD : undefined
    Cpu::op_sbc_a_u8,    // 0xDE : SBC A,d8
    Cpu::op_rst_18h,     // 0xDF : RST 18H
    Cpu::op_ldh_a8_a,    // 0xE0 : LDH (a8),A
    Cpu::op_pop_hl,      // 0xE1 : POP HL
    Cpu::ldh_c_a,        // 0xE2 : LDH (C),A
    Cpu::op_nop,         // 0xE3 : undefined
    Cpu::op_nop,         // 0xE4 : undefined
    Cpu::op_push_hl,     // 0xE5 : PUSH HL
    Cpu::op_and_a_u8,    // 0xE6 : AND d8
    Cpu::op_rst_20h,     // 0xE7 : RST 20H
    Cpu::op_add_sp_r8,   // 0xE8 : ADD SP,r8
    Cpu::op_jp_hl,       // 0xE9 : JP (HL)
    Cpu::op_ld_a16_a,    // 0xEA : LD (a16),A
    Cpu::op_nop,         // 0xEB : undefined
    Cpu::op_nop,         // 0xEC : undefined
    Cpu::op_nop,         // 0xED : undefined
    Cpu::op_xor_a_u8,    // 0xEE : XOR d8
    Cpu::op_rst_28h,     // 0xEF : RST 28H
    Cpu::op_ldh_a_a8,    // 0xF0 : LDH A,(a8)
    Cpu::op_pop_af,      // 0xF1 : POP AF
    Cpu::ldh_a_c,        // 0xF2 : LDH A,(C)
    Cpu::op_di,          // 0xF3 : DI
    Cpu::op_nop,         // 0xF4 : undefined
    Cpu::op_push_af,     // 0xF5 : PUSH AF
    Cpu::op_or_a_u8,     // 0xF6 : OR d8
    Cpu::op_rst_30h,     // 0xF7 : RST 30H
    Cpu::op_ld_hl_sp_i8, // 0xF8 : LD HL,SP+r8
    Cpu::op_ld_sp_hl,    // 0xF9 : LD SP,HL
    Cpu::op_ld_a_a16,    // 0xFA : LD A,(a16)
    Cpu::op_ei,          // 0xFB : EI
    Cpu::op_nop,         // 0xFC : undefined
    Cpu::op_nop,         // 0xFD : undefined
    Cpu::op_cp_a_u8,     // 0xFE : CP d8
    Cpu::op_rst_38h,     // 0xFF : RST 38H
];

const CB_CODE_FUNCTION_TABLE: [fn(&mut Cpu); 256] = [
    Cpu::cb_rlc_b,    // 0x00 : RLC B
    Cpu::cb_rlc_c,    // 0x01 : RLC C
    Cpu::cb_rlc_d,    // 0x02 : RLC D
    Cpu::cb_rlc_e,    // 0x03 : RLC E
    Cpu::cb_rlc_h,    // 0x04 : RLC H
    Cpu::cb_rlc_l,    // 0x05 : RLC L
    Cpu::cb_rlc_hl,   // 0x06 : RLC (HL)
    Cpu::cb_rlc_a,    // 0x07 : RLC A
    Cpu::cb_rrc_b,    // 0x08 : RRC B
    Cpu::cb_rrc_c,    // 0x09 : RRC C
    Cpu::cb_rrc_d,    // 0x0A : RRC D
    Cpu::cb_rrc_e,    // 0x0B : RRC E
    Cpu::cb_rrc_h,    // 0x0C : RRC H
    Cpu::cb_rrc_l,    // 0x0D : RRC L
    Cpu::cb_rrc_hl,   // 0x0E : RRC (HL)
    Cpu::cb_rrc_a,    // 0x0F : RRC A
    Cpu::cb_rl_b,     // 0x10 : RL B
    Cpu::cb_rl_c,     // 0x11 : RL C
    Cpu::cb_rl_d,     // 0x12 : RL D
    Cpu::cb_rl_e,     // 0x13 : RL E
    Cpu::cb_rl_h,     // 0x14 : RL H
    Cpu::cb_rl_l,     // 0x15 : RL L
    Cpu::cb_rl_hl,    // 0x16 : RL (HL)
    Cpu::cb_rl_a,     // 0x17 : RL A
    Cpu::cb_rr_b,     // 0x18 : RR B
    Cpu::cb_rr_c,     // 0x19 : RR C
    Cpu::cb_rr_d,     // 0x1A : RR D
    Cpu::cb_rr_e,     // 0x1B : RR E
    Cpu::cb_rr_h,     // 0x1C : RR H
    Cpu::cb_rr_l,     // 0x1D : RR L
    Cpu::cb_rr_hl,    // 0x1E : RR (HL)
    Cpu::cb_rr_a,     // 0x1F : RR A
    Cpu::cb_sla_b,    // 0x20 : SLA B
    Cpu::cb_sla_c,    // 0x21 : SLA C
    Cpu::cb_sla_d,    // 0x22 : SLA D
    Cpu::cb_sla_e,    // 0x23 : SLA E
    Cpu::cb_sla_h,    // 0x24 : SLA H
    Cpu::cb_sla_l,    // 0x25 : SLA L
    Cpu::cb_sla_hl,   // 0x26 : SLA (HL)
    Cpu::cb_sla_a,    // 0x27 : SLA A
    Cpu::cb_sra_b,    // 0x28 : SRA B
    Cpu::cb_sra_c,    // 0x29 : SRA C
    Cpu::cb_sra_d,    // 0x2A : SRA D
    Cpu::cb_sra_e,    // 0x2B : SRA E
    Cpu::cb_sra_h,    // 0x2C : SRA H
    Cpu::cb_sra_l,    // 0x2D : SRA L
    Cpu::cb_sra_hl,   // 0x2E : SRA (HL)
    Cpu::cb_sra_a,    // 0x2F : SRA A
    Cpu::cb_swap_b,   // 0x30 : SWAP B
    Cpu::cb_swap_c,   // 0x31 : SWAP C
    Cpu::cb_swap_d,   // 0x32 : SWAP D
    Cpu::cb_swap_e,   // 0x33 : SWAP E
    Cpu::cb_swap_h,   // 0x34 : SWAP H
    Cpu::cb_swap_l,   // 0x35 : SWAP L
    Cpu::cb_swap_hl,  // 0x36 : SWAP (HL)
    Cpu::cb_swap_a,   // 0x37 : SWAP A
    Cpu::cb_srl_b,    // 0x38 : SRL B
    Cpu::cb_srl_c,    // 0x39 : SRL C
    Cpu::cb_srl_d,    // 0x3A : SRL D
    Cpu::cb_srl_e,    // 0x3B : SRL E
    Cpu::cb_srl_h,    // 0x3C : SRL H
    Cpu::cb_srl_l,    // 0x3D : SRL L
    Cpu::cb_srl_hl,   // 0x3E : SRL (HL)
    Cpu::cb_srl_a,    // 0x3F : SRL A
    Cpu::cb_bit_0_b,  // 0x40 : BIT 0,B
    Cpu::cb_bit_0_c,  // 0x41 : BIT 0,C
    Cpu::cb_bit_0_d,  // 0x42 : BIT 0,D
    Cpu::cb_bit_0_e,  // 0x43 : BIT 0,E
    Cpu::cb_bit_0_h,  // 0x44 : BIT 0,H
    Cpu::cb_bit_0_l,  // 0x45 : BIT 0,L
    Cpu::cb_bit_0_hl, // 0x46 : BIT 0,(HL)
    Cpu::cb_bit_0_a,  // 0x47 : BIT 0,A
    Cpu::cb_bit_1_b,  // 0x48 : BIT 1,B
    Cpu::cb_bit_1_c,  // 0x49 : BIT 1,C
    Cpu::cb_bit_1_d,  // 0x4A : BIT 1,D
    Cpu::cb_bit_1_e,  // 0x4B : BIT 1,E
    Cpu::cb_bit_1_h,  // 0x4C : BIT 1,H
    Cpu::cb_bit_1_l,  // 0x4D : BIT 1,L
    Cpu::cb_bit_1_hl, // 0x4E : BIT 1,(HL)
    Cpu::cb_bit_1_a,  // 0x4F : BIT 1,A
    Cpu::cb_bit_2_b,  // 0x50 : BIT 2,B
    Cpu::cb_bit_2_c,  // 0x51 : BIT 2,C
    Cpu::cb_bit_2_d,  // 0x52 : BIT 2,D
    Cpu::cb_bit_2_e,  // 0x53 : BIT 2,E
    Cpu::cb_bit_2_h,  // 0x54 : BIT 2,H
    Cpu::cb_bit_2_l,  // 0x55 : BIT 2,L
    Cpu::cb_bit_2_hl, // 0x56 : BIT 2,(HL)
    Cpu::cb_bit_2_a,  // 0x57 : BIT 2,A
    Cpu::cb_bit_3_b,  // 0x58 : BIT 3,B
    Cpu::cb_bit_3_c,  // 0x59 : BIT 3,C
    Cpu::cb_bit_3_d,  // 0x5A : BIT 3,D
    Cpu::cb_bit_3_e,  // 0x5B : BIT 3,E
    Cpu::cb_bit_3_h,  // 0x5C : BIT 3,H
    Cpu::cb_bit_3_l,  // 0x5D : BIT 3,L
    Cpu::cb_bit_3_hl, // 0x5E : BIT 3,(HL)
    Cpu::cb_bit_3_a,  // 0x5F : BIT 3,A
    Cpu::cb_bit_4_b,  // 0x60 : BIT 4,B
    Cpu::cb_bit_4_c,  // 0x61 : BIT 4,C
    Cpu::cb_bit_4_d,  // 0x62 : BIT 4,D
    Cpu::cb_bit_4_e,  // 0x63 : BIT 4,E
    Cpu::cb_bit_4_h,  // 0x64 : BIT 4,H
    Cpu::cb_bit_4_l,  // 0x65 : BIT 4,L
    Cpu::cb_bit_4_hl, // 0x66 : BIT 4,(HL)
    Cpu::cb_bit_4_a,  // 0x67 : BIT 4,A
    Cpu::cb_bit_5_b,  // 0x68 : BIT 5,B
    Cpu::cb_bit_5_c,  // 0x69 : BIT 5,C
    Cpu::cb_bit_5_d,  // 0x6A : BIT 5,D
    Cpu::cb_bit_5_e,  // 0x6B : BIT 5,E
    Cpu::cb_bit_5_h,  // 0x6C : BIT 5,H
    Cpu::cb_bit_5_l,  // 0x6D : BIT 5,L
    Cpu::cb_bit_5_hl, // 0x6E : BIT 5,(HL)
    Cpu::cb_bit_5_a,  // 0x6F : BIT 5,A
    Cpu::cb_bit_6_b,  // 0x70 : BIT 6,B
    Cpu::cb_bit_6_c,  // 0x71 : BIT 6,C
    Cpu::cb_bit_6_d,  // 0x72 : BIT 6,D
    Cpu::cb_bit_6_e,  // 0x73 : BIT 6,E
    Cpu::cb_bit_6_h,  // 0x74 : BIT 6,H
    Cpu::cb_bit_6_l,  // 0x75 : BIT 6,L
    Cpu::cb_bit_6_hl, // 0x76 : BIT 6,(HL)
    Cpu::cb_bit_6_a,  // 0x77 : BIT 6,A
    Cpu::cb_bit_7_b,  // 0x78 : BIT 7,B
    Cpu::cb_bit_7_c,  // 0x79 : BIT 7,C
    Cpu::cb_bit_7_d,  // 0x7A : BIT 7,D
    Cpu::cb_bit_7_e,  // 0x7B : BIT 7,E
    Cpu::cb_bit_7_h,  // 0x7C : BIT 7,H
    Cpu::cb_bit_7_l,  // 0x7D : BIT 7,L
    Cpu::cb_bit_7_hl, // 0x7E : BIT 7,(HL)
    Cpu::cb_bit_7_a,  // 0x7F : BIT 7,A
    Cpu::cb_res_0_b,  // 0x80 : RES 0,B
    Cpu::cb_res_0_c,  // 0x81 : RES 0,C
    Cpu::cb_res_0_d,  // 0x82 : RES 0,D
    Cpu::cb_res_0_e,  // 0x83 : RES 0,E
    Cpu::cb_res_0_h,  // 0x84 : RES 0,H
    Cpu::cb_res_0_l,  // 0x85 : RES 0,L
    Cpu::cb_res_0_hl, // 0x86 : RES 0,(HL)
    Cpu::cb_res_0_a,  // 0x87 : RES 0,A
    Cpu::cb_res_1_b,  // 0x88 : RES 1,B
    Cpu::cb_res_1_c,  // 0x89 : RES 1,C
    Cpu::cb_res_1_d,  // 0x8A : RES 1,D
    Cpu::cb_res_1_e,  // 0x8B : RES 1,E
    Cpu::cb_res_1_h,  // 0x8C : RES 1,H
    Cpu::cb_res_1_l,  // 0x8D : RES 1,L
    Cpu::cb_res_1_hl, // 0x8E : RES 1,(HL)
    Cpu::cb_res_1_a,  // 0x8F : RES 1,A
    Cpu::cb_res_2_b,  // 0x90 : RES 2,B
    Cpu::cb_res_2_c,  // 0x91 : RES 2,C
    Cpu::cb_res_2_d,  // 0x92 : RES 2,D
    Cpu::cb_res_2_e,  // 0x93 : RES 2,E
    Cpu::cb_res_2_h,  // 0x94 : RES 2,H
    Cpu::cb_res_2_l,  // 0x95 : RES 2,L
    Cpu::cb_res_2_hl, // 0x96 : RES 2,(HL)
    Cpu::cb_res_2_a,  // 0x97 : RES 2,A
    Cpu::cb_res_3_b,  // 0x98 : RES 3,B
    Cpu::cb_res_3_c,  // 0x99 : RES 3,C
    Cpu::cb_res_3_d,  // 0x9A : RES 3,D
    Cpu::cb_res_3_e,  // 0x9B : RES 3,E
    Cpu::cb_res_3_h,  // 0x9C : RES 3,H
    Cpu::cb_res_3_l,  // 0x9D : RES 3,L
    Cpu::cb_res_3_hl, // 0x9E : RES 3,(HL)
    Cpu::cb_res_3_a,  // 0x9F : RES 3,A
    Cpu::cb_res_4_b,  // 0xA0 : RES 4,B
    Cpu::cb_res_4_c,  // 0xA1 : RES 4,C
    Cpu::cb_res_4_d,  // 0xA2 : RES 4,D
    Cpu::cb_res_4_e,  // 0xA3 : RES 4,E
    Cpu::cb_res_4_h,  // 0xA4 : RES 4,H
    Cpu::cb_res_4_l,  // 0xA5 : RES 4,L
    Cpu::cb_res_4_hl, // 0xA6 : RES 4,(HL)
    Cpu::cb_res_4_a,  // 0xA7 : RES 4,A
    Cpu::cb_res_5_b,  // 0xA8 : RES 5,B
    Cpu::cb_res_5_c,  // 0xA9 : RES 5,C
    Cpu::cb_res_5_d,  // 0xAA : RES 5,D
    Cpu::cb_res_5_e,  // 0xAB : RES 5,E
    Cpu::cb_res_5_h,  // 0xAC : RES 5,H
    Cpu::cb_res_5_l,  // 0xAD : RES 5,L
    Cpu::cb_res_5_hl, // 0xAE : RES 5,(HL)
    Cpu::cb_res_5_a,  // 0xAF : RES 5,A
    Cpu::cb_res_6_b,  // 0xB0 : RES 6,B
    Cpu::cb_res_6_c,  // 0xB1 : RES 6,C
    Cpu::cb_res_6_d,  // 0xB2 : RES 6,D
    Cpu::cb_res_6_e,  // 0xB3 : RES 6,E
    Cpu::cb_res_6_h,  // 0xB4 : RES 6,H
    Cpu::cb_res_6_l,  // 0xB5 : RES 6,L
    Cpu::cb_res_6_hl, // 0xB6 : RES 6,(HL)
    Cpu::cb_res_6_a,  // 0xB7 : RES 6,A
    Cpu::cb_res_7_b,  // 0xB8 : RES 7,B
    Cpu::cb_res_7_c,  // 0xB9 : RES 7,C
    Cpu::cb_res_7_d,  // 0xBA : RES 7,D
    Cpu::cb_res_7_e,  // 0xBB : RES 7,E
    Cpu::cb_res_7_h,  // 0xBC : RES 7,H
    Cpu::cb_res_7_l,  // 0xBD : RES 7,L
    Cpu::cb_res_7_hl, // 0xBE : RES 7,(HL)
    Cpu::cb_res_7_a,  // 0xBF : RES 7,A
    Cpu::cb_set_0_b,  // 0xC0 : SET 0,B
    Cpu::cb_set_0_c,  // 0xC1 : SET 0,C
    Cpu::cb_set_0_d,  // 0xC2 : SET 0,D
    Cpu::cb_set_0_e,  // 0xC3 : SET 0,E
    Cpu::cb_set_0_h,  // 0xC4 : SET 0,H
    Cpu::cb_set_0_l,  // 0xC5 : SET 0,L
    Cpu::cb_set_0_hl, // 0xC6 : SET 0,(HL)
    Cpu::cb_set_0_a,  // 0xC7 : SET 0,A
    Cpu::cb_set_1_b,  // 0xC8 : SET 1,B
    Cpu::cb_set_1_c,  // 0xC9 : SET 1,C
    Cpu::cb_set_1_d,  // 0xCA : SET 1,D
    Cpu::cb_set_1_e,  // 0xCB : SET 1,E
    Cpu::cb_set_1_h,  // 0xCC : SET 1,H
    Cpu::cb_set_1_l,  // 0xCD : SET 1,L
    Cpu::cb_set_1_hl, // 0xCE : SET 1,(HL)
    Cpu::cb_set_1_a,  // 0xCF : SET 1,A
    Cpu::cb_set_2_b,  // 0xD0 : SET 2,B
    Cpu::cb_set_2_c,  // 0xD1 : SET 2,C
    Cpu::cb_set_2_d,  // 0xD2 : SET 2,D
    Cpu::cb_set_2_e,  // 0xD3 : SET 2,E
    Cpu::cb_set_2_h,  // 0xD4 : SET 2,H
    Cpu::cb_set_2_l,  // 0xD5 : SET 2,L
    Cpu::cb_set_2_hl, // 0xD6 : SET 2,(HL)
    Cpu::cb_set_2_a,  // 0xD7 : SET 2,A
    Cpu::cb_set_3_b,  // 0xD8 : SET 3,B
    Cpu::cb_set_3_c,  // 0xD9 : SET 3,C
    Cpu::cb_set_3_d,  // 0xDA : SET 3,D
    Cpu::cb_set_3_e,  // 0xDB : SET 3,E
    Cpu::cb_set_3_h,  // 0xDC : SET 3,H
    Cpu::cb_set_3_l,  // 0xDD : SET 3,L
    Cpu::cb_set_3_hl, // 0xDE : SET 3,(HL)
    Cpu::cb_set_3_a,  // 0xDF : SET 3,A
    Cpu::cb_set_4_b,  // 0xE0 : SET 4,B
    Cpu::cb_set_4_c,  // 0xE1 : SET 4,C
    Cpu::cb_set_4_d,  // 0xE2 : SET 4,D
    Cpu::cb_set_4_e,  // 0xE3 : SET 4,E
    Cpu::cb_set_4_h,  // 0xE4 : SET 4,H
    Cpu::cb_set_4_l,  // 0xE5 : SET 4,L
    Cpu::cb_set_4_hl, // 0xE6 : SET 4,(HL)
    Cpu::cb_set_4_a,  // 0xE7 : SET 4,A
    Cpu::cb_set_5_b,  // 0xE8 : SET 5,B
    Cpu::cb_set_5_c,  // 0xE9 : SET 5,C
    Cpu::cb_set_5_d,  // 0xEA : SET 5,D
    Cpu::cb_set_5_e,  // 0xEB : SET 5,E
    Cpu::cb_set_5_h,  // 0xEC : SET 5,H
    Cpu::cb_set_5_l,  // 0xED : SET 5,L
    Cpu::cb_set_5_hl, // 0xEE : SET 5,(HL)
    Cpu::cb_set_5_a,  // 0xEF : SET 5,A
    Cpu::cb_set_6_b,  // 0xF0 : SET 6,B
    Cpu::cb_set_6_c,  // 0xF1 : SET 6,C
    Cpu::cb_set_6_d,  // 0xF2 : SET 6,D
    Cpu::cb_set_6_e,  // 0xF3 : SET 6,E
    Cpu::cb_set_6_h,  // 0xF4 : SET 6,H
    Cpu::cb_set_6_l,  // 0xF5 : SET 6,L
    Cpu::cb_set_6_hl, // 0xF6 : SET 6,(HL)
    Cpu::cb_set_6_a,  // 0xF7 : SET 6,A
    Cpu::cb_set_7_b,  // 0xF8 : SET 7,B
    Cpu::cb_set_7_c,  // 0xF9 : SET 7,C
    Cpu::cb_set_7_d,  // 0xFA : SET 7,D
    Cpu::cb_set_7_e,  // 0xFB : SET 7,E
    Cpu::cb_set_7_h,  // 0xFC : SET 7,H
    Cpu::cb_set_7_l,  // 0xFD : SET 7,L
    Cpu::cb_set_7_hl, // 0xFE : SET 7,(HL)
    Cpu::cb_set_7_a,  // 0xFF : SET 7,A
];

const STATUS_FLAG_Z: u8 = 0x80;
const STATUS_FLAG_N: u8 = 0x40;
const STATUS_FLAG_H: u8 = 0x20;
const STATUS_FLAG_C: u8 = 0x10;

struct CpuRegisters {
    register_a: u8,
    register_b: u8,
    register_c: u8,
    register_d: u8,
    register_e: u8,
    register_h: u8,
    register_l: u8,
}

impl CpuRegisters {
    fn new() -> Self {
        Self {
            register_a: 0,
            register_b: 0,
            register_c: 0,
            register_d: 0,
            register_e: 0,
            register_h: 0,
            register_l: 0,
        }
    }

    fn bc(&self) -> u16 {
        ((self.register_b as u16) << 8) | self.register_c as u16
    }

    fn set_bc(&mut self, value: u16) {
        self.register_b = (value >> 8) as u8;
        self.register_c = (value & 0xFF) as u8;
    }

    fn de(&self) -> u16 {
        ((self.register_d as u16) << 8) | self.register_e as u16
    }

    fn set_de(&mut self, value: u16) {
        self.register_d = (value >> 8) as u8;
        self.register_e = (value & 0xFF) as u8;
    }

    fn hl(&self) -> u16 {
        ((self.register_h as u16) << 8) | self.register_l as u16
    }

    fn set_hl(&mut self, value: u16) {
        self.register_h = (value >> 8) as u8;
        self.register_l = (value & 0xFF) as u8;
    }
}

pub struct Cpu {
    memory: Box<dyn Memory>,
    program_counter: u16,

    registers: CpuRegisters,
    status_flags: u8,
    stack_pointer: u16,

    interrupt_master_enabled: bool,
}

impl Cpu {
    pub fn new(memory: Box<dyn Memory>) -> Self {
        Self {
            memory,
            program_counter: 0,
            registers: CpuRegisters::new(),
            status_flags: 0,
            stack_pointer: 0,
            interrupt_master_enabled: false,
        }
    }

    pub fn tick(&mut self) {
        OP_CODE_FUNCTION_TABLE[self.fetch_u8() as usize](self);
    }

    fn read_hl(&mut self) -> u8 {
        self.memory.read(self.registers.hl())
    }

    fn write_hl(&mut self, value: u8) {
        self.memory.write(self.registers.hl(), value);
    }

    fn op_placeholder(&mut self) {
        panic!("Opcode not implemented!")
    }

    fn fetch_u8(&mut self) -> u8 {
        let value = self.memory.read(self.program_counter);
        self.program_counter += 1;

        value
    }

    fn fetch_u16(&mut self) -> u16 {
        let lsb = self.fetch_u8();
        let msb = self.fetch_u8();

        (lsb as u16) | ((msb as u16) << 8)
    }

    fn stack_pop_u8(&mut self) -> u8 {
        let value = self.memory.read(self.stack_pointer);
        self.stack_pointer = self.stack_pointer.wrapping_add(1);

        value
    }

    fn stack_push_u8(&mut self, value: u8) {
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
        self.memory.write(self.stack_pointer, value);
    }

    fn stack_pop_u16(&mut self) -> u16 {
        // TODO check LSB/MSB order
        (self.stack_pop_u8() as u16) | ((self.stack_pop_u8() as u16) << 8)
    }

    fn stack_push_u16(&mut self, value: u16) {
        self.stack_push_u8((value >> 8) as u8);
        self.stack_push_u8((value & 0xFF) as u8);
    }

    fn run_add_u8_and_update_flags(&mut self, operand: u8) {
        let result: u16 = (self.registers.register_a as u16) + (operand as u16);
        self.status_flags = 0;

        if (result & 0xFF) == 0 {
            self.status_flags |= STATUS_FLAG_Z;
        }

        if ((self.registers.register_a & 0xF) + (operand & 0xF)) > 0xF {
            self.status_flags |= STATUS_FLAG_H;
        }

        if result > 0xFF {
            self.status_flags |= STATUS_FLAG_C;
        }

        self.registers.register_a = (result & 0xFF) as u8;
    }

    // TODO: This op is supposed to be 2 machine cycles long, is there a dummy cycle?
    fn run_add_u16_and_update_flags(&mut self, operand: u16) {
        let hl = self.registers.hl();
        let result: u32 = (hl as u32) + (operand as u32);

        self.status_flags &= !(STATUS_FLAG_N | STATUS_FLAG_C | STATUS_FLAG_H);

        if (((hl & 0xFFF) + (operand & 0xFFF)) & 0x1000) != 0 {
            self.status_flags |= STATUS_FLAG_H;
        }

        if (result & 0x10000) != 0 {
            self.status_flags |= STATUS_FLAG_C;
        }

        self.registers.set_hl((result & 0xFFFF) as u16);
    }

    fn run_adc_and_update_flags(&mut self, operand: u8) {
        let carry: u16 = if (self.status_flags & STATUS_FLAG_C) != 0 {
            1
        } else {
            0
        };

        let result: u16 = (self.registers.register_a as u16) + (operand as u16) + carry;

        self.status_flags = 0;

        if (result & 0xFF) == 0 {
            self.status_flags |= STATUS_FLAG_Z;
        }

        if ((self.registers.register_a & 0xF) + (operand & 0xF) + (carry as u8)) > 0xF {
            self.status_flags |= STATUS_FLAG_H;
        }

        if result > 0xFF {
            self.status_flags |= STATUS_FLAG_C;
        }

        self.registers.register_a = (result & 0xFF) as u8;
    }

    fn run_sub_and_update_flags(&mut self, operand: u8) {
        self.status_flags = STATUS_FLAG_N;

        if self.registers.register_a == operand {
            self.status_flags |= STATUS_FLAG_Z;
        }

        if (self.registers.register_a & 0xF) < (operand & 0xF) {
            self.status_flags |= STATUS_FLAG_H;
        }

        if self.registers.register_a < operand {
            self.status_flags |= STATUS_FLAG_C;
        }

        self.registers.register_a = self.registers.register_a.wrapping_sub(operand);
    }

    fn run_sbc_and_update_flags(&mut self, operand: u8) {
        let carry: u8 = if (self.status_flags & STATUS_FLAG_C) != 0 {
            1
        } else {
            0
        };

        let result = self
            .registers
            .register_a
            .wrapping_sub(operand)
            .wrapping_sub(carry);

        self.status_flags = STATUS_FLAG_N;

        if result == 0 {
            self.status_flags |= STATUS_FLAG_Z;
        }

        if (self.registers.register_a & 0xF) < ((operand & 0xF) + carry) {
            self.status_flags |= STATUS_FLAG_H;
        }

        if (self.registers.register_a as u16)
            .wrapping_sub(operand as u16)
            .wrapping_sub(carry as u16)
            > 0xFF
        {
            self.status_flags |= STATUS_FLAG_C;
        }

        self.registers.register_a = result;
    }

    fn run_and_and_update_flags(&mut self, operand: u8) {
        self.registers.register_a &= operand;
        self.status_flags = STATUS_FLAG_H;

        if self.registers.register_a == 0 {
            self.status_flags |= STATUS_FLAG_Z;
        }
    }

    fn run_xor_and_update_flags(&mut self, operand: u8) {
        self.registers.register_a ^= operand;
        self.status_flags = STATUS_FLAG_H;

        if self.registers.register_a == 0 {
            self.status_flags |= STATUS_FLAG_Z;
        }
    }

    fn run_or_and_update_flags(&mut self, operand: u8) {
        self.registers.register_a |= operand;
        self.status_flags = STATUS_FLAG_H;

        if self.registers.register_a == 0 {
            self.status_flags |= STATUS_FLAG_Z;
        }
    }

    fn run_cp_and_update_flags(&mut self, operand: u8) {
        self.status_flags = STATUS_FLAG_N;

        if self.registers.register_a == operand {
            self.status_flags |= STATUS_FLAG_Z;
        }

        if (self.registers.register_a & 0xF) < (operand & 0xF) {
            self.status_flags |= STATUS_FLAG_H;
        }

        if self.registers.register_a < operand {
            self.status_flags |= STATUS_FLAG_C;
        }
    }

    fn run_inc_u8_and_update_flags(&mut self, operand: u8) -> u8 {
        let result = operand.wrapping_add(1);
        self.status_flags &= !(STATUS_FLAG_N | STATUS_FLAG_Z | STATUS_FLAG_H);

        if result == 0 {
            self.status_flags |= STATUS_FLAG_Z;
        }

        if (result & 0xF) == 0 {
            self.status_flags |= STATUS_FLAG_H;
        }

        result
    }

    fn run_dec_u8_and_update_flags(&mut self, operand: u8) -> u8 {
        let result = operand.wrapping_sub(1);

        self.status_flags &= !(STATUS_FLAG_Z | STATUS_FLAG_H);
        self.status_flags |= STATUS_FLAG_N;

        if result == 0 {
            self.status_flags |= STATUS_FLAG_Z;
        }

        if (result & 0xF) == 0 {
            self.status_flags |= STATUS_FLAG_H;
        }

        result
    }

    fn run_rlc_u8_and_update_flags(&mut self, operand: u8) -> u8 {
        let carry = (operand & 0x80) != 0;
        let result = operand.wrapping_shl(1) | carry as u8;

        self.status_flags = 0;

        if carry {
            self.status_flags |= STATUS_FLAG_C;
        }

        if operand == 0 {
            self.status_flags |= STATUS_FLAG_Z;
        }

        result
    }

    fn run_rrc_u8_and_update_flags(&mut self, operand: u8) -> u8 {
        let carry = (operand & 0x01) != 0;
        let result = operand.wrapping_shr(1) | ((carry as u8) << 7);

        self.status_flags = 0;

        if carry {
            self.status_flags |= STATUS_FLAG_C;
        }

        if operand == 0 {
            self.status_flags |= STATUS_FLAG_Z;
        }

        result
    }

    fn run_rl_u8_and_update_flags(&mut self, operand: u8) -> u8 {
        let carry = (operand & 0x80) != 0;
        let result = operand.wrapping_shl(1) | ((self.status_flags & STATUS_FLAG_C) != 0) as u8;

        self.status_flags = 0;

        if carry {
            self.status_flags |= STATUS_FLAG_C;
        }

        if operand == 0 {
            self.status_flags |= STATUS_FLAG_Z;
        }

        result
    }

    fn run_rr_u8_and_update_flags(&mut self, operand: u8) -> u8 {
        let carry = (operand & 0x01) != 0;
        let result =
            operand.wrapping_shr(1) | ((((self.status_flags & STATUS_FLAG_C) != 0) as u8) << 7);

        self.status_flags = 0;

        if carry {
            self.status_flags |= STATUS_FLAG_C;
        }

        if operand == 0 {
            self.status_flags |= STATUS_FLAG_Z;
        }

        result
    }

    fn run_sla_u8_and_update_flags(&mut self, operand: u8) -> u8 {
        let carry = (operand & 0x80) != 0;
        self.status_flags = 0;

        if carry {
            self.status_flags |= STATUS_FLAG_C;
        }

        if (operand & 0x7F) == 0 {
            self.status_flags |= STATUS_FLAG_Z;
        }

        operand.wrapping_shl(1)
    }

    fn run_sra_u8_and_update_flags(&mut self, operand: u8) -> u8 {
        let carry = (operand & 0x01) != 0;
        let result = operand.wrapping_shr(1) | (operand & 0x80);

        self.status_flags = 0;

        if carry {
            self.status_flags |= STATUS_FLAG_C;
        }

        if result == 0 {
            self.status_flags |= STATUS_FLAG_Z;
        }

        result
    }

    fn run_swap_u8_and_update_flags(&mut self, operand: u8) -> u8 {
        let result = (operand >> 4) | (operand << 4);
        self.status_flags = 0;

        if result == 0 {
            self.status_flags |= STATUS_FLAG_Z;
        }

        result
    }

    fn run_srl_u8_and_update_flags(&mut self, operand: u8) -> u8 {
        let result = operand >> 1;
        self.status_flags = 0;

        if (operand & 0x01) != 0 {
            self.status_flags |= STATUS_FLAG_C;
        }

        if result == 0 {
            self.status_flags |= STATUS_FLAG_Z;
        }

        result
    }

    fn run_bit_u8_and_update_flags(&mut self, operand: u8, bit: u8) {
        self.status_flags &= STATUS_FLAG_C;
        self.status_flags |= STATUS_FLAG_H;

        if (operand & bit) == 0 {
            self.status_flags |= STATUS_FLAG_Z;
        }
    }
}

/// Gameboy SM63 opcode implementations
impl Cpu {
    /// Opcode 0x00: [NOP](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=34)
    ///
    /// No operation. This instruction doesn’t do anything, but can be used to add a
    /// delay of one machine cycle and increment PC by one (1 machine cycle).
    fn op_nop(&mut self) {}

    /// Opcode 0x01: [LD BC,d16](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=34)
    ///
    /// Load to the 16-bit register BC, the immediate 16-bit data following the opcode
    /// (3 machine cycles).
    fn op_ld_bc_u16(&mut self) {
        let value = self.fetch_u16();
        self.registers.set_bc(value);
    }

    /// Opcode 0x02: [LD (BC),A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=22)
    ///
    /// Load to the absolute address specified by the 16-bit register BC, data from the
    /// 8-bit A register (2 machine cycles).
    fn op_ld_bc_a(&mut self) {
        self.memory
            .write(self.registers.bc(), self.registers.register_a);
    }

    /// Opcode 0x03: [INC BC](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=72)
    ///
    /// Increments data in the 16-bit register BC (2 machine cycles).
    fn op_inc_bc(&mut self) {
        // TODO: add extra dummy cycle?
        self.registers.set_bc(self.registers.bc().wrapping_add(1));
    }

    /// Opcode 0x04: [INC B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=55)
    ///
    /// Increments data in the 8-bit register B (1 machine cycle).
    fn op_inc_b(&mut self) {
        self.registers.register_b = self.run_inc_u8_and_update_flags(self.registers.register_b);
    }

    /// Opcode 0x05: [DEC B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=57)
    ///
    /// Decrements data in the 8-bit register B (1 machine cycle).
    fn op_dec_b(&mut self) {
        self.registers.register_b = self.run_dec_u8_and_update_flags(self.registers.register_b);
    }

    /// Opcode 0x06: [LD B,d8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=16)
    ///
    /// Load to the 8-bit register B, the immediate data following the opcode (2
    /// machine cycles).
    fn op_ld_b_d8(&mut self) {
        self.registers.register_b = self.fetch_u8();
    }

    /// Opcode 0x07: [RLCA](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=77)
    ///
    /// Rotate the 8-bit register B to the left (1 machine cycle).
    fn op_rlca(&mut self) {
        let carry = (self.registers.register_a & 0x80) != 0;
        self.registers.register_a = self.registers.register_a.wrapping_shl(1);
        self.status_flags = 0;

        if carry {
            self.status_flags |= STATUS_FLAG_C;
            self.registers.register_a |= 0x01;
        }
    }

    /// Opcode 0x08: [LD (a16),SP](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=35)
    ///
    /// Load to the absolute address specified by the 16-bit operand following the
    /// opcode, data from the 16-bit SP register (5 machine cycles).
    fn op_ld_u16_sp(&mut self) {
        let address = self.fetch_u16();

        self.memory
            .write(address, (self.stack_pointer & 0xFF) as u8);
        self.memory
            .write(address + 1, (self.stack_pointer >> 8) as u8);
    }

    /// Opcode 0x09: [ADD HL,BC](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=35)
    ///
    /// Adds to the 16-bit HL register pair, the 16-bit register BC, and stores the
    /// result back into the HL register pair (2 machine cycles).
    fn op_add_hl_bc(&mut self) {
        self.run_add_u16_and_update_flags(self.registers.bc());
    }

    /// Opcode 0x0A: [LD A,(BC)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=20)
    ///
    /// Load to the 8-bit A register, data from the absolute address specified by the
    /// 16-bit register BC (2 machine cycles).
    fn op_ld_a_bc(&mut self) {
        self.registers.register_a = self.memory.read(self.registers.bc());
    }

    /// Opcode 0x0B: [DEC BC](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=73)
    ///
    /// DEcrements data in the 16-bit register BC (2 machine cycles).
    fn op_dec_bc(&mut self) {
        // TODO: add extra dummy cycle?
        self.registers.set_bc(self.registers.bc().wrapping_sub(1));
    }

    /// Opcode 0x0C: [INC C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=55)
    ///
    /// Increments data in the 8-bit register C (1 machine cycle).
    fn op_inc_c(&mut self) {
        self.registers.register_c = self.run_inc_u8_and_update_flags(self.registers.register_c);
    }

    /// Opcode 0x0D: [DEC C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=57)
    ///
    /// Decrements data in the 8-bit register C (1 machine cycle).
    fn op_dec_c(&mut self) {
        self.registers.register_c = self.run_dec_u8_and_update_flags(self.registers.register_c);
    }

    /// Opcode 0x0E: [LD C,d8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=16)
    ///
    /// Load to the 8-bit register C, the immediate data following the opcode (2
    /// machine cycles).
    fn op_ld_c_d8(&mut self) {
        self.registers.register_c = self.fetch_u8();
    }

    /// Opcode 0x0F: [RRCA](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=78)
    ///
    /// Rotate the 8-bit register B to the right (1 machine cycle).
    fn op_rrca(&mut self) {
        let carry = (self.registers.register_a & 0x01) != 0;
        self.registers.register_a = self.registers.register_a.wrapping_shr(1);
        self.status_flags = 0;

        if carry {
            self.status_flags |= STATUS_FLAG_C;
            self.registers.register_a |= 0x80;
        }
    }

    /// Opcode 0x11: [LD DE,d16](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=34)
    ///
    /// Load to the 16-bit register DE, the immediate 16-bit data following the opcode
    /// (3 machine cycles).
    fn op_ld_de_u16(&mut self) {
        let value = self.fetch_u16();
        self.registers.set_de(value);
    }

    /// Opcode 0x12: [LD (DE),A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=23)
    ///
    /// Load to the absolute address specified by the 16-bit register DE, data from the
    /// 8-bit A register (2 machine cycles).
    fn op_ld_de_a(&mut self) {
        self.memory
            .write(self.registers.de(), self.registers.register_a);
    }

    /// Opcode 0x13: [INC DE](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=72)
    ///
    /// Increments data in the 16-bit register DE (2 machine cycles).
    fn op_inc_de(&mut self) {
        // TODO: add extra dummy cycle?
        self.registers.set_de(self.registers.de().wrapping_add(1));
    }

    /// Opcode 0x14: [INC D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=55)
    ///
    /// Increments data in the 8-bit register D (1 machine cycle).
    fn op_inc_d(&mut self) {
        self.registers.register_d = self.run_inc_u8_and_update_flags(self.registers.register_d);
    }

    /// Opcode 0x15: [DEC D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=57)
    ///
    /// Decrements data in the 8-bit register D (1 machine cycle).
    fn op_dec_d(&mut self) {
        self.registers.register_d = self.run_dec_u8_and_update_flags(self.registers.register_d);
    }

    /// Opcode 0x16: [LD D,d8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=16)
    ///
    /// Load to the 8-bit register D, the immediate data following the opcode (2
    /// machine cycles).
    fn op_ld_d_d8(&mut self) {
        self.registers.register_d = self.fetch_u8();
    }

    /// Opcode 0x17: [RLA](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=79)
    ///
    /// Rotate the 8-bit register B to the left (1 machine cycle).
    fn op_rla(&mut self) {
        let carry = (self.registers.register_a & 0x80) != 0;
        self.registers.register_a = self.registers.register_a.wrapping_shl(1);
        self.status_flags = 0;

        if (self.status_flags & STATUS_FLAG_C) != 0 {
            self.registers.register_a |= 0x01;
        }

        if carry {
            self.status_flags |= STATUS_FLAG_C;
        }
    }

    /// Opcode 0x17: [JR r8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=108)
    ///
    /// Unconditional jump to the relative address specified by the signed 8-bit
    /// operand following the opcode (3 machine cycles).
    fn op_jr_r8(&mut self) {
        let offset = self.fetch_u8() as i8;
        self.program_counter = self.program_counter.wrapping_add_signed(offset as i16);
    }

    /// Opcode 0x19: [ADD HL,DE](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=35)
    ///
    /// Adds to the 16-bit HL register pair, the 16-bit register DE, and stores the
    /// result back into the HL register pair (2 machine cycles).
    fn op_add_hl_de(&mut self) {
        self.run_add_u16_and_update_flags(self.registers.de());
    }

    /// Opcode 0x1A: [LD A,(DE)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=20)
    ///
    /// Load to the 8-bit A register, data from the absolute address specified by the
    /// 16-bit register DE (2 machine cycles).
    fn op_ld_a_de(&mut self) {
        self.registers.register_a = self.memory.read(self.registers.de());
    }

    /// Opcode 0x1B: [DEC DE](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=73)
    ///
    /// DEcrements data in the 16-bit register DE (2 machine cycles).
    fn op_dec_de(&mut self) {
        // TODO: add extra dummy cycle?
        self.registers.set_de(self.registers.de().wrapping_sub(1));
    }

    /// Opcode 0x1C: [INC E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=55)
    ///
    /// Increments data in the 8-bit register E (1 machine cycle).
    fn op_inc_e(&mut self) {
        self.registers.register_e = self.run_inc_u8_and_update_flags(self.registers.register_e);
    }

    /// Opcode 0x1D: [DEC E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=57)
    ///
    /// Decrements data in the 8-bit register E (1 machine cycle).
    fn op_dec_e(&mut self) {
        self.registers.register_e = self.run_dec_u8_and_update_flags(self.registers.register_e);
    }

    /// Opcode 0x1E: [LD E,d8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=16)
    ///
    /// Load to the 8-bit register E, the immediate data following the opcode (2
    /// machine cycles).
    fn op_ld_e_d8(&mut self) {
        self.registers.register_e = self.fetch_u8();
    }

    /// Opcode 0x1F: [RRA](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=80)
    ///
    /// Rotate the 8-bit register B to the right (1 machine cycle).
    fn op_rra(&mut self) {
        let carry = (self.registers.register_a & 0x01) != 0;
        self.registers.register_a = self.registers.register_a.wrapping_shr(1);
        self.status_flags = 0;

        if (self.status_flags & STATUS_FLAG_C) != 0 {
            self.registers.register_a |= 0x80;
        }

        if carry {
            self.status_flags |= STATUS_FLAG_C;
        }
    }

    /// Opcode 0x20: [JR NZ,r8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=109)
    ///
    /// Conditional jump to the relative address specified by the signed 8-bit operand
    /// following the opcode, depending on the condition NZ. Note that the operand
    /// (relative address offset) is read even when the condition is false (2/3 machine
    /// cycles).
    fn op_jp_nz_r8(&mut self) {
        let offset = self.fetch_u8() as i8;

        if (self.status_flags & STATUS_FLAG_Z) == 0 {
            // TODO: add extra cycle
            self.program_counter = self.program_counter.wrapping_add_signed(offset as i16);
        }
    }

    /// Opcode 0x21: [LD HL,d16](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=34)
    ///
    /// Load to the 16-bit register HL, the immediate 16-bit data following the opcode
    /// (3 machine cycles).
    fn op_ld_hl_u16(&mut self) {
        let value = self.fetch_u16();
        self.registers.set_hl(value);
    }

    /// Opcode 0x22: [LD (HL+),A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=33)
    ///
    /// Load to the absolute address specified by the 16-bit register HL, data from the
    /// 8-bit A register. The value of HL is decremented after the memory write (2
    /// machine cycles).
    fn op_ld_hl_inc_a(&mut self) {
        let address = self.registers.hl();
        self.memory.write(address, self.registers.register_a);
        self.registers.set_hl(address + 1);
    }

    /// Opcode 0x23: [INC HL](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=72)
    ///
    /// Increments data in the 16-bit register HL (2 machine cycles).
    fn op_inc_hl(&mut self) {
        // TODO: add extra dummy cycle?
        self.registers.set_hl(self.registers.hl().wrapping_add(1));
    }

    /// Opcode 0x24: [INC H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=55)
    ///
    /// Increments data in the 8-bit register H (1 machine cycle).
    fn op_inc_h(&mut self) {
        self.registers.register_h = self.run_inc_u8_and_update_flags(self.registers.register_h);
    }

    /// Opcode 0x25: [DEC H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=57)
    ///
    /// Decrements data in the 8-bit register H (1 machine cycle).
    fn op_dec_h(&mut self) {
        self.registers.register_h = self.run_dec_u8_and_update_flags(self.registers.register_h);
    }

    /// Opcode 0x26: [LD H,d8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=16)
    ///
    /// Load to the 8-bit register H, the immediate data following the opcode (2
    /// machine cycles).
    fn op_ld_h_d8(&mut self) {
        self.registers.register_h = self.fetch_u8();
    }

    /// Opcode 0x27: [DAA](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=70)
    ///
    /// Adjusts the contents of the accumulator (A register) to hold the correct packed
    /// BCD result after an arithmetic operation on packed BCD numbers (1 machine
    /// cycle).
    fn op_daa(&mut self) {
        let mut current_value = self.registers.register_a as i16;

        self.status_flags &= !(STATUS_FLAG_Z | STATUS_FLAG_H);

        if (self.status_flags & STATUS_FLAG_N) != 0 {
            if (self.status_flags & STATUS_FLAG_H) != 0 {
                current_value = current_value.wrapping_sub(0x06) & 0xFF;
            }

            if (self.status_flags & STATUS_FLAG_C) != 0 {
                current_value = current_value.wrapping_sub(0x60);
            }
        } else {
            if (self.status_flags & STATUS_FLAG_H) != 0 || (current_value & 0x0F) > 0x09 {
                current_value = current_value.wrapping_add(0x06);
            }

            if (self.status_flags & STATUS_FLAG_C) != 0 || current_value > 0x9F {
                current_value = current_value.wrapping_add(0x60);
            }
        }

        if (current_value & 0xFF) == 0 {
            self.status_flags |= STATUS_FLAG_Z;
        }

        if (current_value & 0x0100) == 0x100 {
            self.status_flags |= STATUS_FLAG_C;
        }

        self.registers.register_a = (current_value & 0xFF) as u8;
    }

    /// Opcode 0x28: [JR Z,r8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=109)
    ///
    /// Conditional jump to the relative address specified by the signed 8-bit operand
    /// following the opcode, depending on the condition Z. Note that the operand
    /// (relative address offset) is read even when the condition is false (2/3 machine
    /// cycles).
    fn op_jp_z_r8(&mut self) {
        let offset = self.fetch_u8() as i8;

        if (self.status_flags & STATUS_FLAG_Z) != 0 {
            // TODO: add extra cycle
            self.program_counter = self.program_counter.wrapping_add_signed(offset as i16);
        }
    }

    /// Opcode 0x29: [ADD HL,HL](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=35)
    ///
    /// Adds to the 16-bit HL register pair, the 16-bit register HL, and stores the
    /// result back into the HL register pair (2 machine cycles).
    fn op_add_hl_hl(&mut self) {
        self.run_add_u16_and_update_flags(self.registers.hl());
    }

    /// Opcode 0x2A: [LD A,(HL+)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=32)
    ///
    /// Load to the 8-bit A register, data from the absolute address specified by the
    /// 16-bit register HL. The value of HL is incremented after the memory read (2
    /// machine cycles).
    fn op_ld_a_hl_inc(&mut self) {
        let address = self.registers.hl();
        self.registers.register_a = self.memory.read(address);
        self.registers.set_hl(address + 1);
    }

    /// Opcode 0x2B: [DEC HL](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=73)
    ///
    /// DEcrements data in the 16-bit register HL (2 machine cycles).
    fn op_dec_hl(&mut self) {
        // TODO: add extra dummy cycle?
        self.registers.set_hl(self.registers.hl().wrapping_sub(1));
    }

    /// Opcode 0x2C: [INC L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=55)
    ///
    /// Increments data in the 8-bit register L (1 machine cycle).
    fn op_inc_l(&mut self) {
        self.registers.register_l = self.run_inc_u8_and_update_flags(self.registers.register_l);
    }

    /// Opcode 0x2D: [DEC L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=57)
    ///
    /// Decrements data in the 8-bit register L (1 machine cycle).
    fn op_dec_l(&mut self) {
        self.registers.register_l = self.run_dec_u8_and_update_flags(self.registers.register_l);
    }

    /// Opcode 0x2E: [LD L,d8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=16)
    ///
    /// Load to the 8-bit register L, the immediate data following the opcode (2
    /// machine cycles).
    fn op_ld_l_d8(&mut self) {
        self.registers.register_l = self.fetch_u8();
    }

    /// Opcode 0x2F: [CPL](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=71)
    ///
    /// Flips all the bits in the 8-bit A register, and sets the N and H flags (1
    /// machine cycle).
    fn op_cpl(&mut self) {
        self.registers.register_a = !self.registers.register_a;
        self.status_flags |= STATUS_FLAG_H | STATUS_FLAG_N;
    }

    /// Opcode 0x30: [JR NC,r8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=109)
    ///
    /// Conditional jump to the relative address specified by the signed 8-bit operand
    /// following the opcode, depending on the condition NC. Note that the operand
    /// (relative address offset) is read even when the condition is false (2/3 machine
    /// cycles).
    fn op_jp_nc_r8(&mut self) {
        let offset = self.fetch_u8() as i8;

        if (self.status_flags & STATUS_FLAG_C) == 0 {
            // TODO: add extra cycle
            self.program_counter = self.program_counter.wrapping_add_signed(offset as i16);
        }
    }

    /// Opcode 0x31: [LD SP,d16](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=34)
    ///
    /// Load to the 16-bit register SP, the immediate 16-bit data following the opcode
    /// (3 machine cycles).
    fn op_ld_sp_u16(&mut self) {
        self.stack_pointer = self.fetch_u16();
    }

    /// Opcode 0x32: [LD (HL-),A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=31)
    ///
    /// Load to the absolute address specified by the 16-bit register HL, data from the
    /// 8-bit A register. The value of HL is decremented after the memory write (2
    /// machine cycles).
    fn op_ld_hl_dec_a(&mut self) {
        let address = self.registers.hl();
        self.memory.write(address, self.registers.register_a);
        self.registers.set_hl(address - 1);
    }

    /// Opcode 0x33: [INC SP](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=72)
    ///
    /// Increments data in the 16-bit register SP (2 machine cycles).
    fn op_inc_sp(&mut self) {
        // TODO: add extra dummy cycle?
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
    }

    /// Opcode 0x34: [INC (HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=56)
    ///
    /// Increments data at the absolute address specified by the 16-bit register HL (3
    /// machine cycles).
    fn op_inc_hl_ind(&mut self) {
        let address = self.registers.hl();
        let value = self.memory.read(address);
        let value = self.run_inc_u8_and_update_flags(value);
        self.memory.write(address, value);
    }

    /// Opcode 0x35: [DEC (HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=58)
    ///
    /// Decrements data at the absolute address specified by the 16-bit register HL (3
    /// machine cycles).
    fn op_dec_hl_ind(&mut self) {
        let address = self.registers.hl();
        let value = self.memory.read(address);
        let value = self.run_dec_u8_and_update_flags(value);
        self.memory.write(address, value);
    }

    /// Opcode 0x36: [LD (HL),d8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=19)
    ///
    /// Load to the absolute address specified by the 16-bit register HL, the immediate
    /// data following the opcode (3 machine cycles).
    fn op_ld_hl_d8(&mut self) {
        let value = self.fetch_u8();
        self.memory.write(self.registers.hl(), value);
    }

    /// Opcode 0x37: [SCF](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=69)
    ///
    /// Sets the carry flag, and clears the N and H flags (1 machine cycle).
    fn op_scf(&mut self) {
        self.status_flags |= STATUS_FLAG_C;
        self.status_flags &= !(STATUS_FLAG_H | STATUS_FLAG_N);
    }

    /// Opcode 0x38: [JR C,r8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=109)
    ///
    /// Conditional jump to the relative address specified by the signed 8-bit operand
    /// following the opcode, depending on the condition C. Note that the operand
    /// (relative address offset) is read even when the condition is false (2/3 machine
    /// cycles).
    fn op_jp_c_r8(&mut self) {
        let offset = self.fetch_u8() as i8;

        if (self.status_flags & STATUS_FLAG_C) != 0 {
            // TODO: add extra cycle
            self.program_counter = self.program_counter.wrapping_add_signed(offset as i16);
        }
    }

    /// Opcode 0x39: [ADD HL,SP](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=35)
    ///
    /// Adds to the 16-bit HL register pair, the 16-bit register SP, and stores the
    /// result back into the HL register pair (2 machine cycles).
    fn op_add_hl_sp(&mut self) {
        self.run_add_u16_and_update_flags(self.stack_pointer);
    }

    /// Opcode 0x3A: [LD A,(HL-)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=30)
    ///
    /// Load to the 8-bit A register, data from the absolute address specified by the
    /// 16-bit register HL. The value of HL is decremented after the memory read (2
    /// machine cycles).
    fn op_ld_a_hl_dec(&mut self) {
        let address = self.registers.hl();
        self.registers.register_a = self.memory.read(address);
        self.registers.set_hl(address - 1);
    }

    /// Opcode 0x3B: [DEC SP](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=73)
    ///
    /// DEcrements data in the 16-bit register SP (2 machine cycles).
    fn op_dec_sp(&mut self) {
        // TODO: add extra dummy cycle?
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    /// Opcode 0x3C: [INC A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=55)
    ///
    /// Increments data in the 8-bit register A (1 machine cycle).
    fn op_inc_a(&mut self) {
        self.registers.register_a = self.run_inc_u8_and_update_flags(self.registers.register_a);
    }

    /// Opcode 0x3D: [DEC A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=57)
    ///
    /// Decrements data in the 8-bit register A (1 machine cycle).
    fn op_dec_a(&mut self) {
        self.registers.register_a = self.run_dec_u8_and_update_flags(self.registers.register_a);
    }

    /// Opcode 0x3E: [LD A,d8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=16)
    ///
    /// Load to the 8-bit register A, the immediate data following the opcode (2
    /// machine cycles).
    fn op_ld_a_d8(&mut self) {
        self.registers.register_a = self.fetch_u8();
    }

    /// Opcode 0x3F: [CCF](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=68)
    ///
    /// Flips the carry flag, and clears the N and H flags (1 machine cycle).
    fn op_ccf(&mut self) {
        self.status_flags ^= STATUS_FLAG_C;
        self.status_flags &= !(STATUS_FLAG_H | STATUS_FLAG_N);
    }

    /// Opcode 0x40: [LD B,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register B, data from the 8-bit register B (1 machine cycle).
    fn op_ld_b_b(&mut self) {}

    /// Opcode 0x41: [LD B,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register B, data from the 8-bit register C (1 machine cycle).
    fn op_ld_b_c(&mut self) {
        self.registers.register_b = self.registers.register_c;
    }

    /// Opcode 0x42: [LD B,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register B, data from the 8-bit register D (1 machine cycle).
    fn op_ld_b_d(&mut self) {
        self.registers.register_b = self.registers.register_d;
    }

    /// Opcode 0x43: [LD B,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register B, data from the 8-bit register E (1 machine cycle).
    fn op_ld_b_e(&mut self) {
        self.registers.register_b = self.registers.register_e;
    }

    /// Opcode 0x44: [LD B,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register B, data from the 8-bit register H (1 machine cycle).
    fn op_ld_b_h(&mut self) {
        self.registers.register_b = self.registers.register_h;
    }

    /// Opcode 0x45: [LD B,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register B, data from the 8-bit register L (1 machine cycle).
    fn op_ld_b_l(&mut self) {
        self.registers.register_b = self.registers.register_l;
    }

    /// Opcode 0x46: [LD B,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=17)
    ///
    /// Load to the 8-bit register B, data from the absolute address specified by the
    /// 16-bit register HL (2 machine cycle).
    fn op_ld_b_hl(&mut self) {
        self.registers.register_b = self.read_hl();
    }

    /// Opcode 0x47: [LD B,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register B, data from the 8-bit register A (1 machine cycle).
    fn op_ld_b_a(&mut self) {
        self.registers.register_b = self.registers.register_a;
    }

    /// Opcode 0x48: [LD C,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register C, data from the 8-bit register B (1 machine cycle).
    fn op_ld_c_b(&mut self) {
        self.registers.register_c = self.registers.register_b;
    }

    /// Opcode 0x49: [LD C,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register C, data from the 8-bit register C (1 machine cycle).
    fn op_ld_c_c(&mut self) {}

    /// Opcode 0x4A: [LD C,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register C, data from the 8-bit register D (1 machine cycle).
    fn op_ld_c_d(&mut self) {
        self.registers.register_c = self.registers.register_d;
    }

    /// Opcode 0x4B: [LD C,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register C, data from the 8-bit register E (1 machine cycle).
    fn op_ld_c_e(&mut self) {
        self.registers.register_c = self.registers.register_e;
    }

    /// Opcode 0x4C: [LD C,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register C, data from the 8-bit register H (1 machine cycle).
    fn op_ld_c_h(&mut self) {
        self.registers.register_c = self.registers.register_h;
    }

    /// Opcode 0x4D: [LD C,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register C, data from the 8-bit register L (1 machine cycle).
    fn op_ld_c_l(&mut self) {
        self.registers.register_c = self.registers.register_l;
    }

    /// Opcode 0x4E: [LD C,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=17)
    ///
    /// Load to the 8-bit register C, data from the absolute address specified by the
    /// 16-bit register HL (2 machine cycle).
    fn op_ld_c_hl(&mut self) {
        self.registers.register_c = self.read_hl();
    }

    /// Opcode 0x4F: [LD C,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register C, data from the 8-bit register A (1 machine cycle).
    fn op_ld_c_a(&mut self) {
        self.registers.register_c = self.registers.register_a;
    }

    /// Opcode 0x50: [LD D,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register D, data from the 8-bit register B (1 machine cycle).
    fn op_ld_d_b(&mut self) {
        self.registers.register_d = self.registers.register_b;
    }

    /// Opcode 0x51: [LD D,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register D, data from the 8-bit register C (1 machine cycle).
    fn op_ld_d_c(&mut self) {
        self.registers.register_d = self.registers.register_c;
    }

    /// Opcode 0x52: [LD D,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register D, data from the 8-bit register D (1 machine cycle).
    fn op_ld_d_d(&mut self) {}

    /// Opcode 0x53: [LD D,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register D, data from the 8-bit register E (1 machine cycle).
    fn op_ld_d_e(&mut self) {
        self.registers.register_d = self.registers.register_e;
    }

    /// Opcode 0x54: [LD D,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register D, data from the 8-bit register H (1 machine cycle).
    fn op_ld_d_h(&mut self) {
        self.registers.register_d = self.registers.register_h;
    }

    /// Opcode 0x55: [LD D,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register D, data from the 8-bit register L (1 machine cycle).
    fn op_ld_d_l(&mut self) {
        self.registers.register_d = self.registers.register_l;
    }

    /// Opcode 0x56: [LD D,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=17)
    ///
    /// Load to the 8-bit register D, data from the absolute address specified by the
    /// 16-bit register HL (2 machine cycle).
    fn op_ld_d_hl(&mut self) {
        self.registers.register_d = self.read_hl();
    }

    /// Opcode 0x57: [LD D,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register D, data from the 8-bit register A (1 machine cycle).
    fn op_ld_d_a(&mut self) {
        self.registers.register_d = self.registers.register_a;
    }

    /// Opcode 0x58: [LD E,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register E, data from the 8-bit register B (1 machine cycle).
    fn op_ld_e_b(&mut self) {
        self.registers.register_e = self.registers.register_b;
    }

    /// Opcode 0x59: [LD E,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register E, data from the 8-bit register C (1 machine cycle).
    fn op_ld_e_c(&mut self) {
        self.registers.register_e = self.registers.register_c;
    }

    /// Opcode 0x5A: [LD E,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register E, data from the 8-bit register D (1 machine cycle).
    fn op_ld_e_d(&mut self) {
        self.registers.register_e = self.registers.register_d;
    }

    /// Opcode 0x5B: [LD E,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register E, data from the 8-bit register E (1 machine cycle).
    fn op_ld_e_e(&mut self) {}

    /// Opcode 0x5C: [LD E,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register E, data from the 8-bit register H (1 machine cycle).
    fn op_ld_e_h(&mut self) {
        self.registers.register_e = self.registers.register_h;
    }

    /// Opcode 0x5D: [LD E,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register E, data from the 8-bit register L (1 machine cycle).
    fn op_ld_e_l(&mut self) {
        self.registers.register_e = self.registers.register_l;
    }

    /// Opcode 0x5E: [LD E,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=17)
    ///
    /// Load to the 8-bit register E, data from the absolute address specified by the
    /// 16-bit register HL (2 machine cycle).
    fn op_ld_e_hl(&mut self) {
        self.registers.register_e = self.read_hl();
    }

    /// Opcode 0x5F: [LD E,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register E, data from the 8-bit register A (1 machine cycle).
    fn op_ld_e_a(&mut self) {
        self.registers.register_e = self.registers.register_a;
    }

    /// Opcode 0x60: [LD H,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register H, data from the 8-bit register B (1 machine cycle).
    fn op_ld_h_b(&mut self) {
        self.registers.register_h = self.registers.register_b;
    }

    /// Opcode 0x61: [LD H,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register H, data from the 8-bit register C (1 machine cycle).
    fn op_ld_h_c(&mut self) {
        self.registers.register_h = self.registers.register_c;
    }

    /// Opcode 0x62: [LD H,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register H, data from the 8-bit register D (1 machine cycle).
    fn op_ld_h_d(&mut self) {
        self.registers.register_h = self.registers.register_d;
    }

    /// Opcode 0x63: [LD H,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register H, data from the 8-bit register E (1 machine cycle).
    fn op_ld_h_e(&mut self) {
        self.registers.register_h = self.registers.register_e;
    }

    /// Opcode 0x64: [LD H,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register H, data from the 8-bit register H (1 machine cycle).
    fn op_ld_h_h(&mut self) {}

    /// Opcode 0x65: [LD H,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register H, data from the 8-bit register L (1 machine cycle).
    fn op_ld_h_l(&mut self) {
        self.registers.register_h = self.registers.register_l;
    }

    /// Opcode 0x66: [LD H,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=17)
    ///
    /// Load to the 8-bit register H, data from the absolute address specified by the
    /// 16-bit register HL (2 machine cycle).
    fn op_ld_h_hl(&mut self) {
        self.registers.register_h = self.read_hl();
    }

    /// Opcode 0x67: [LD H,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register H, data from the 8-bit register A (1 machine cycle).
    fn op_ld_h_a(&mut self) {
        self.registers.register_h = self.registers.register_a;
    }

    /// Opcode 0x68: [LD L,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register L, data from the 8-bit register B (1 machine cycle).
    fn op_ld_l_b(&mut self) {
        self.registers.register_l = self.registers.register_b;
    }

    /// Opcode 0x69: [LD L,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register L, data from the 8-bit register C (1 machine cycle).
    fn op_ld_l_c(&mut self) {
        self.registers.register_l = self.registers.register_c;
    }

    /// Opcode 0x6A: [LD L,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register L, data from the 8-bit register D (1 machine cycle).
    fn op_ld_l_d(&mut self) {
        self.registers.register_l = self.registers.register_d;
    }

    /// Opcode 0x6B: [LD L,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register L, data from the 8-bit register E (1 machine cycle).
    fn op_ld_l_e(&mut self) {
        self.registers.register_l = self.registers.register_e;
    }

    /// Opcode 0x6C: [LD L,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register L, data from the 8-bit register H (1 machine cycle).
    fn op_ld_l_h(&mut self) {
        self.registers.register_l = self.registers.register_h;
    }

    /// Opcode 0x6D: [LD L,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register L, data from the 8-bit register L (1 machine cycle).
    fn op_ld_l_l(&mut self) {}

    /// Opcode 0x6E: [LD L,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=17)
    ///
    /// Load to the 8-bit register L, data from the absolute address specified by the
    /// 16-bit register HL (2 machine cycle).
    fn op_ld_l_hl(&mut self) {
        self.registers.register_l = self.read_hl();
    }

    /// Opcode 0x6F: [LD L,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register L, data from the 8-bit register A (1 machine cycle).
    fn op_ld_l_a(&mut self) {
        self.registers.register_l = self.registers.register_a;
    }

    /// Opcode 0x70: [LD (HL),B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the absolute address specified by the 16-bit register HL, data from the
    /// 8-bit register B (2 machine cycles).
    fn op_ld_hl_b(&mut self) {
        self.write_hl(self.registers.register_b);
    }

    /// Opcode 0x71: [LD (HL),C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the absolute address specified by the 16-bit register HL, data from the
    /// 8-bit register C (2 machine cycles).
    fn op_ld_hl_c(&mut self) {
        self.write_hl(self.registers.register_c);
    }

    /// Opcode 0x72: [LD (HL),D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the absolute address specified by the 16-bit register HL, data from the
    /// 8-bit register D (2 machine cycles).
    fn op_ld_hl_d(&mut self) {
        self.write_hl(self.registers.register_d);
    }

    /// Opcode 0x73: [LD (HL),E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the absolute address specified by the 16-bit register HL, data from the
    /// 8-bit register E (2 machine cycles).
    fn op_ld_hl_e(&mut self) {
        self.write_hl(self.registers.register_e);
    }

    /// Opcode 0x74: [LD (HL),H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the absolute address specified by the 16-bit register HL, data from the
    /// 8-bit register H (2 machine cycles).
    fn op_ld_hl_h(&mut self) {
        self.write_hl(self.registers.register_h);
    }

    /// Opcode 0x75: [LD (HL),L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the absolute address specified by the 16-bit register HL, data from the
    /// 8-bit register L (2 machine cycles).
    fn op_ld_hl_l(&mut self) {
        self.write_hl(self.registers.register_l);
    }

    /// Opcode 0x75: [HALT](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=118)
    fn op_halt(&mut self) {
        panic!("Opcode not implemented!");
    }

    /// Opcode 0x77: [LD (HL),A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the absolute address specified by the 16-bit register HL, data from the
    /// 8-bit register A (2 machine cycles).
    fn op_ld_hl_a(&mut self) {
        self.write_hl(self.registers.register_a);
    }

    /// Opcode 0x78: [LD A,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register A, data from the 8-bit register B (1 machine cycle).
    fn op_ld_a_b(&mut self) {
        self.registers.register_a = self.registers.register_b;
    }

    /// Opcode 0x79: [LD A,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register A, data from the 8-bit register C (1 machine cycle).
    fn op_ld_a_c(&mut self) {
        self.registers.register_a = self.registers.register_c;
    }

    /// Opcode 0x7A: [LD A,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register A, data from the 8-bit register D (1 machine cycle).
    fn op_ld_a_d(&mut self) {
        self.registers.register_a = self.registers.register_d;
    }

    /// Opcode 0x7B: [LD A,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register A, data from the 8-bit register E (1 machine cycle).
    fn op_ld_a_e(&mut self) {
        self.registers.register_a = self.registers.register_e;
    }

    /// Opcode 0x7C: [LD A,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register A, data from the 8-bit register H (1 machine cycle).
    fn op_ld_a_h(&mut self) {
        self.registers.register_a = self.registers.register_h;
    }

    /// Opcode 0x7D: [LD A,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register A, data from the 8-bit register L (1 machine cycle).
    fn op_ld_a_l(&mut self) {
        self.registers.register_a = self.registers.register_l;
    }

    /// Opcode 0x7E: [LD A,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=17)
    ///
    /// Load to the 8-bit register A, data from the absolute address specified by the
    /// 16-bit register HL (2 machine cycle).
    fn op_ld_a_hl(&mut self) {
        self.registers.register_a = self.read_hl();
    }

    /// Opcode 0x7F: [LD A,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register A, data from the 8-bit register A (1 machine cycle).
    fn op_ld_a_a(&mut self) {}

    /// Opcode 0x80: [ADD A,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=40)
    ///
    /// Adds to the 8-bit A register, the 8-bit register B, and stores the result back
    /// into the A register (1 machine cycle).
    fn op_add_a_b(&mut self) {
        self.run_add_u8_and_update_flags(self.registers.register_b);
    }

    /// Opcode 0x81: [ADD A,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=40)
    ///
    /// Adds to the 8-bit A register, the 8-bit register C, and stores the result back
    /// into the A register (1 machine cycle).
    fn op_add_a_c(&mut self) {
        self.run_add_u8_and_update_flags(self.registers.register_c);
    }

    /// Opcode 0x82: [ADD A,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=40)
    ///
    /// Adds to the 8-bit A register, the 8-bit register D, and stores the result back
    /// into the A register (1 machine cycle).
    fn op_add_a_d(&mut self) {
        self.run_add_u8_and_update_flags(self.registers.register_d);
    }

    /// Opcode 0x83: [ADD A,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=40)
    ///
    /// Adds to the 8-bit A register, the 8-bit register E, and stores the result back
    /// into the A register (1 machine cycle).
    fn op_add_a_e(&mut self) {
        self.run_add_u8_and_update_flags(self.registers.register_e);
    }

    /// Opcode 0x84: [ADD A,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=40)
    ///
    /// Adds to the 8-bit A register, the 8-bit register H, and stores the result back
    /// into the A register (1 machine cycle).
    fn op_add_a_h(&mut self) {
        self.run_add_u8_and_update_flags(self.registers.register_h);
    }

    /// Opcode 0x85: [ADD A,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=40)
    ///
    /// Adds to the 8-bit A register, the 8-bit register L, and stores the result back
    /// into the A register (1 machine cycle).
    fn op_add_a_l(&mut self) {
        self.run_add_u8_and_update_flags(self.registers.register_l);
    }

    /// Opcode 0x86: [ADD A,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=41)
    ///
    /// Adds to the 8-bit A register, data from the absolute address specified by the
    /// 16-bit register HL, and stores the result back into the A register (2 machine
    /// cycles).
    fn op_add_a_hl(&mut self) {
        let operand = self.read_hl();
        self.run_add_u8_and_update_flags(operand);
    }

    /// Opcode 0x87: [ADD A,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=40)
    ///
    /// Adds to the 8-bit A register, the 8-bit register A, and stores the result back
    /// into the A register (1 machine cycle).
    fn op_add_a_a(&mut self) {
        self.run_add_u8_and_update_flags(self.registers.register_a);
    }

    /// Opcode 0x88: [ADC A,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=43)
    ///
    /// Adds to the 8-bit A register, the carry flag and the 8-bit register b, and
    /// stores the result back into the A register (1 machine cycle).
    fn op_adc_a_b(&mut self) {
        self.run_adc_and_update_flags(self.registers.register_b);
    }

    /// Opcode 0x89: [ADC A,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=43)
    ///
    /// Adds to the 8-bit A register, the carry flag and the 8-bit register C, and
    /// stores the result back into the A register (1 machine cycle).
    fn op_adc_a_c(&mut self) {
        self.run_adc_and_update_flags(self.registers.register_c);
    }

    /// Opcode 0x8A: [ADC A,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=43)
    ///
    /// Adds to the 8-bit A register, the carry flag and the 8-bit register D, and
    /// stores the result back into the A register (1 machine cycle).
    fn op_adc_a_d(&mut self) {
        self.run_adc_and_update_flags(self.registers.register_d);
    }

    /// Opcode 0x8B: [ADC A,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=43)
    ///
    /// Adds to the 8-bit A register, the carry flag and the 8-bit register E, and
    /// stores the result back into the A register (1 machine cycle).
    fn op_adc_a_e(&mut self) {
        self.run_adc_and_update_flags(self.registers.register_e);
    }

    /// Opcode 0x8C: [ADC A,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=43)
    ///
    /// Adds to the 8-bit A register, the carry flag and the 8-bit register H, and
    /// stores the result back into the A register (1 machine cycle).
    fn op_adc_a_h(&mut self) {
        self.run_adc_and_update_flags(self.registers.register_h);
    }

    /// Opcode 0x8D: [ADC A,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=43)
    ///
    /// Adds to the 8-bit A register, the carry flag and the 8-bit register L, and
    /// stores the result back into the A register (1 machine cycle).
    fn op_adc_a_l(&mut self) {
        self.run_adc_and_update_flags(self.registers.register_l);
    }

    /// Opcode 0x8E: [ADC A,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=44)
    ///
    /// Adds to the 8-bit A register, the carry flag and data from the absolute address
    /// specified by the 16-bit register HL, and stores the result back into the A
    /// register (2 machine cycles).
    fn op_adc_a_hl(&mut self) {
        let operand = self.read_hl();
        self.run_adc_and_update_flags(operand);
    }

    /// Opcode 0x8F: [ADC A,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=43)
    ///
    /// Adds to the 8-bit A register, the carry flag and the 8-bit register A, and
    /// stores the result back into the A register (1 machine cycle).
    fn op_adc_a_a(&mut self) {
        self.run_adc_and_update_flags(self.registers.register_a);
    }

    /// Opcode 0x90: [SUB B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=46)
    ///
    /// Subtracts from the 8-bit A register, the 8-bit register B, and stores the
    /// result back into the A register (1 machine cycle).
    fn op_sub_a_b(&mut self) {
        self.run_sub_and_update_flags(self.registers.register_b);
    }

    /// Opcode 0x91: [SUB C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=46)
    ///
    /// Subtracts from the 8-bit A register, the 8-bit register C, and stores the
    /// result back into the A register (1 machine cycle).
    fn op_sub_a_c(&mut self) {
        self.run_sub_and_update_flags(self.registers.register_c);
    }

    /// Opcode 0x92: [SUB D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=46)
    ///
    /// Subtracts from the 8-bit A register, the 8-bit register D, and stores the
    /// result back into the A register (1 machine cycle).
    fn op_sub_a_d(&mut self) {
        self.run_sub_and_update_flags(self.registers.register_d);
    }

    /// Opcode 0x93: [SUB E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=46)
    ///
    /// Subtracts from the 8-bit A register, the 8-bit register E, and stores the
    /// result back into the A register (1 machine cycle).
    fn op_sub_a_e(&mut self) {
        self.run_sub_and_update_flags(self.registers.register_e);
    }

    /// Opcode 0x94: [SUB H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=46)
    ///
    /// Subtracts from the 8-bit A register, the 8-bit register H, and stores the
    /// result back into the A register (1 machine cycle).
    fn op_sub_a_h(&mut self) {
        self.run_sub_and_update_flags(self.registers.register_h);
    }

    /// Opcode 0x95: [SUB L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=46)
    ///
    /// Subtracts from the 8-bit A register, the 8-bit register L, and stores the
    /// result back into the A register (1 machine cycle).
    fn op_sub_a_l(&mut self) {
        self.run_sub_and_update_flags(self.registers.register_l);
    }

    /// Opcode 0x96: [SUB (HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=47)
    ///
    /// Subtracts from the 8-bit A register, data from the absolute address specified
    /// by the 16-bit register HL, and stores the result back into the A register (2
    /// machine cycles).
    fn op_sub_a_hl(&mut self) {
        let operand = self.read_hl();
        self.run_sub_and_update_flags(operand);
    }

    /// Opcode 0x97: [SUB A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=46)
    ///
    /// Subtracts from the 8-bit A register, the 8-bit register A, and stores the
    /// result back into the A register (1 machine cycle).
    fn op_sub_a_a(&mut self) {
        self.run_sub_and_update_flags(self.registers.register_a);
    }

    /// Opcode 0x98: [SBC A,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=49)
    ///
    /// Subtracts from the 8-bit A register, the carry flag and the 8-bit register B,
    /// and stores the result back into the A register (1 machine cycle).
    fn op_sbc_a_b(&mut self) {
        self.run_sbc_and_update_flags(self.registers.register_b);
    }

    /// Opcode 0x99: [SBC A,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=49)
    ///
    /// Subtracts from the 8-bit A register, the carry flag and the 8-bit register C,
    /// and stores the result back into the A register (1 machine cycle).
    fn op_sbc_a_c(&mut self) {
        self.run_sbc_and_update_flags(self.registers.register_c);
    }

    /// Opcode 0x9A: [SBC A,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=49)
    ///
    /// Subtracts from the 8-bit A register, the carry flag and the 8-bit register D,
    /// and stores the result back into the A register (1 machine cycle).
    fn op_sbc_a_d(&mut self) {
        self.run_sbc_and_update_flags(self.registers.register_d);
    }

    /// Opcode 0x9B: [SBC A,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=49)
    ///
    /// Subtracts from the 8-bit A register, the carry flag and the 8-bit register E,
    /// and stores the result back into the A register (1 machine cycle).
    fn op_sbc_a_e(&mut self) {
        self.run_sbc_and_update_flags(self.registers.register_e);
    }

    /// Opcode 0x9C: [SBC A,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=49)
    ///
    /// Subtracts from the 8-bit A register, the carry flag and the 8-bit register H,
    /// and stores the result back into the A register (1 machine cycle).
    fn op_sbc_a_h(&mut self) {
        self.run_sbc_and_update_flags(self.registers.register_h);
    }

    /// Opcode 0x9D: [SBC A,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=49)
    ///
    /// Subtracts from the 8-bit A register, the carry flag and the 8-bit register L,
    /// and stores the result back into the A register (1 machine cycle).
    fn op_sbc_a_l(&mut self) {
        self.run_sbc_and_update_flags(self.registers.register_l);
    }

    /// Opcode 0x9E: [SBC A,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=50)
    ///
    /// Subtracts from the 8-bit A register, the carry flag and data from the absolute
    /// address specified by the 16-bit register HL, and stores the result back into
    /// the A register (2 machine cycles).
    fn op_sbc_a_hl(&mut self) {
        let operand = self.read_hl();
        self.run_sbc_and_update_flags(operand);
    }

    /// Opcode 0x9F: [SBC A,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=49)
    ///
    /// Subtracts from the 8-bit A register, the carry flag and the 8-bit register A,
    /// and stores the result back into the A register (1 machine cycle).
    fn op_sbc_a_a(&mut self) {
        self.run_sbc_and_update_flags(self.registers.register_a);
    }

    /// Opcode 0xA0: [AND B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=59)
    ///
    /// Performs a bitwise AND operation between the 8-bit A register and the 8-bit
    /// register B, and stores the result back into the A register (1 machine cycle).
    fn op_and_a_b(&mut self) {
        self.run_and_and_update_flags(self.registers.register_b);
    }

    /// Opcode 0xA1: [AND C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=59)
    ///
    /// Performs a bitwise AND operation between the 8-bit A register and the 8-bit
    /// register C, and stores the result back into the A register (1 machine cycle).
    fn op_and_a_c(&mut self) {
        self.run_and_and_update_flags(self.registers.register_c);
    }

    /// Opcode 0xA2: [AND D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=59)
    ///
    /// Performs a bitwise AND operation between the 8-bit A register and the 8-bit
    /// register D, and stores the result back into the A register (1 machine cycle).
    fn op_and_a_d(&mut self) {
        self.run_and_and_update_flags(self.registers.register_d);
    }

    /// Opcode 0xA3: [AND E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=59)
    ///
    /// Performs a bitwise AND operation between the 8-bit A register and the 8-bit
    /// register E, and stores the result back into the A register (1 machine cycle).
    fn op_and_a_e(&mut self) {
        self.run_and_and_update_flags(self.registers.register_e);
    }

    /// Opcode 0xA4: [AND H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=59)
    ///
    /// Performs a bitwise AND operation between the 8-bit A register and the 8-bit
    /// register H, and stores the result back into the A register (1 machine cycle).
    fn op_and_a_h(&mut self) {
        self.run_and_and_update_flags(self.registers.register_h);
    }

    /// Opcode 0xA5: [AND L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=59)
    ///
    /// Performs a bitwise AND operation between the 8-bit A register and the 8-bit
    /// register L, and stores the result back into the A register (1 machine cycle).
    fn op_and_a_l(&mut self) {
        self.run_and_and_update_flags(self.registers.register_l);
    }

    /// Opcode 0xA6: [AND (HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=60)
    ///
    /// Performs a bitwise AND operation between the 8-bit A register and data from the
    /// absolute address specified by the 16-bit register HL, and stores the result
    /// back into the A register (2 machine cycles).
    fn op_and_a_hl(&mut self) {
        let operand = self.read_hl();
        self.run_and_and_update_flags(operand);
    }

    /// Opcode 0xA7: [AND A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=59)
    ///
    /// Performs a bitwise AND operation between the 8-bit A register and the 8-bit
    /// register A, and stores the result back into the A register (1 machine cycle).
    fn op_and_a_a(&mut self) {
        self.run_and_and_update_flags(self.registers.register_a);
    }

    /// Opcode 0xA8: [XOR B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=65)
    ///
    /// Performs a bitwise XOR operation between the 8-bit A register and the 8-bit
    /// register B, and stores the result back into the A register (1 machine cycle).
    fn op_xor_a_b(&mut self) {
        self.run_xor_and_update_flags(self.registers.register_b);
    }

    /// Opcode 0xA9: [XOR C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=65)
    ///
    /// Performs a bitwise XOR operation between the 8-bit A register and the 8-bit
    /// register C, and stores the result back into the A register (1 machine cycle).
    fn op_xor_a_c(&mut self) {
        self.run_xor_and_update_flags(self.registers.register_c);
    }

    /// Opcode 0xAA: [XOR D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=65)
    ///
    /// Performs a bitwise XOR operation between the 8-bit A register and the 8-bit
    /// register D, and stores the result back into the A register (1 machine cycle).
    fn op_xor_a_d(&mut self) {
        self.run_xor_and_update_flags(self.registers.register_d);
    }

    /// Opcode 0xAB: [XOR E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=65)
    ///
    /// Performs a bitwise XOR operation between the 8-bit A register and the 8-bit
    /// register E, and stores the result back into the A register (1 machine cycle).
    fn op_xor_a_e(&mut self) {
        self.run_xor_and_update_flags(self.registers.register_e);
    }

    /// Opcode 0xAC: [XOR H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=65)
    ///
    /// Performs a bitwise XOR operation between the 8-bit A register and the 8-bit
    /// register H, and stores the result back into the A register (1 machine cycle).
    fn op_xor_a_h(&mut self) {
        self.run_xor_and_update_flags(self.registers.register_h);
    }

    /// Opcode 0xAD: [XOR L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=65)
    ///
    /// Performs a bitwise XOR operation between the 8-bit A register and the 8-bit
    /// register L, and stores the result back into the A register (1 machine cycle).
    fn op_xor_a_l(&mut self) {
        self.run_xor_and_update_flags(self.registers.register_l);
    }

    /// Opcode 0xAE: [XOR (HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=66)
    ///
    /// Performs a bitwise XOR operation between the 8-bit A register and data from the
    /// absolute address specified by the 16-bit register HL, and stores the result
    /// back into the A register (2 machine cycles).
    fn op_xor_a_hl(&mut self) {
        let operand = self.read_hl();
        self.run_xor_and_update_flags(operand);
    }

    /// Opcode 0xAF: [XOR A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=65)
    ///
    /// Performs a bitwise XOR operation between the 8-bit A register and the 8-bit
    /// register A, and stores the result back into the A register (1 machine cycle).
    fn op_xor_a_a(&mut self) {
        self.run_xor_and_update_flags(self.registers.register_a);
    }

    /// Opcode 0xB0: [OR B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=62)
    ///
    /// Performs a bitwise OR operation between the 8-bit A register and the 8-bit
    /// register B, and stores the result back into the A register (1 machine cycle).
    fn op_or_a_b(&mut self) {
        self.run_or_and_update_flags(self.registers.register_b);
    }

    /// Opcode 0xB1: [OR C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=62)
    ///
    /// Performs a bitwise OR operation between the 8-bit A register and the 8-bit
    /// register C, and stores the result back into the A register (1 machine cycle).
    fn op_or_a_c(&mut self) {
        self.run_or_and_update_flags(self.registers.register_c);
    }

    /// Opcode 0xB2: [OR D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=62)
    ///
    /// Performs a bitwise OR operation between the 8-bit A register and the 8-bit
    /// register D, and stores the result back into the A register (1 machine cycle).
    fn op_or_a_d(&mut self) {
        self.run_or_and_update_flags(self.registers.register_d);
    }

    /// Opcode 0xB3: [OR E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=62)
    ///
    /// Performs a bitwise OR operation between the 8-bit A register and the 8-bit
    /// register E, and stores the result back into the A register (1 machine cycle).
    fn op_or_a_e(&mut self) {
        self.run_or_and_update_flags(self.registers.register_e);
    }

    /// Opcode 0xB4: [OR H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=62)
    ///
    /// Performs a bitwise OR operation between the 8-bit A register and the 8-bit
    /// register h, and stores the result back into the A register (1 machine cycle).
    fn op_or_a_h(&mut self) {
        self.run_or_and_update_flags(self.registers.register_h);
    }

    /// Opcode 0xB5: [OR L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=62)
    ///
    /// Performs a bitwise OR operation between the 8-bit A register and the 8-bit
    /// register L, and stores the result back into the A register (1 machine cycle).
    fn op_or_a_l(&mut self) {
        self.run_or_and_update_flags(self.registers.register_l);
    }

    /// Opcode 0xB6: [OR (HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=63)
    ///
    /// Performs a bitwise OR operation between the 8-bit A register and data from the
    /// absolute address specified by the 16-bit register HL, and stores the result
    /// back into the A register (2 machine cycles).
    fn op_or_a_hl(&mut self) {
        let operand = self.read_hl();
        self.run_or_and_update_flags(operand);
    }

    /// Opcode 0xB7: [OR A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=62)
    ///
    /// Performs a bitwise OR operation between the 8-bit A register and the 8-bit
    /// register A, and stores the result back into the A register (1 machine cycle).
    fn op_or_a_a(&mut self) {
        self.run_or_and_update_flags(self.registers.register_a);
    }

    /// Opcode 0xB8: [CP B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=52)
    ///
    /// Subtracts from the 8-bit A register, the 8-bit register B, and updates flags
    /// based on the result. This instruction is basically identical to SUB B, but does
    /// not update the A register (1 machine cycle).
    fn op_cp_a_b(&mut self) {
        self.run_cp_and_update_flags(self.registers.register_b);
    }

    /// Opcode 0xB9: [CP C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=52)
    ///
    /// Subtracts from the 8-bit A register, the 8-bit register C, and updates flags
    /// based on the result. This instruction is basically identical to SUB C, but does
    /// not update the A register (1 machine cycle).
    fn op_cp_a_c(&mut self) {
        self.run_cp_and_update_flags(self.registers.register_c);
    }

    /// Opcode 0xBA: [CP D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=52)
    ///
    /// Subtracts from the 8-bit A register, the 8-bit register D, and updates flags
    /// based on the result. This instruction is basically identical to SUB D, but does
    /// not update the A register (1 machine cycle).
    fn op_cp_a_d(&mut self) {
        self.run_cp_and_update_flags(self.registers.register_d);
    }

    /// Opcode 0xBB: [CP E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=52)
    ///
    /// Subtracts from the 8-bit A register, the 8-bit register E, and updates flags
    /// based on the result. This instruction is basically identical to SUB E, but does
    /// not update the A register (1 machine cycle).
    fn op_cp_a_e(&mut self) {
        self.run_cp_and_update_flags(self.registers.register_e);
    }

    /// Opcode 0xBC: [CP H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=52)
    ///
    /// Subtracts from the 8-bit A register, the 8-bit register H, and updates flags
    /// based on the result. This instruction is basically identical to SUB H, but does
    /// not update the A register (1 machine cycle).
    fn op_cp_a_h(&mut self) {
        self.run_cp_and_update_flags(self.registers.register_h);
    }

    /// Opcode 0xBD: [CP L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=52)
    ///
    /// Subtracts from the 8-bit A register, the 8-bit register L, and updates flags
    /// based on the result. This instruction is basically identical to SUB L, but does
    /// not update the A register (1 machine cycle).
    fn op_cp_a_l(&mut self) {
        self.run_cp_and_update_flags(self.registers.register_l);
    }

    /// Opcode 0xBE: [CP (HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=53)
    ///
    /// Subtracts from the 8-bit A register, data from the absolute address specified
    /// by the 16-bit register HL, and updates flags based on the result. This
    /// instruction is basically identical to SUB (HL), but does not update the A
    /// register (2 machine cycles).
    fn op_cp_a_hl(&mut self) {
        let operand: u8 = self.read_hl();
        self.run_cp_and_update_flags(operand);
    }

    /// Opcode 0xBF: [CP A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=52)
    ///
    /// Subtracts from the 8-bit A register, the 8-bit register A, and updates flags
    /// based on the result. This instruction is basically identical to SUB A, but does
    /// not update the A register (1 machine cycle).
    fn op_cp_a_a(&mut self) {
        self.run_cp_and_update_flags(self.registers.register_a);
    }

    /// Opcode 0xC0: [RET NZ](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=115)
    ///
    /// Conditional return from a function, depending on the condition NZ (2/5 machine
    /// cycles).
    fn op_ret_nz(&mut self) {
        // TODO: extra dummy cycle
        if (self.status_flags & STATUS_FLAG_Z) == 0 {
            // TODO: extra dummy cycle
            self.program_counter = self.stack_pop_u16();
        }
    }

    /// Opcode 0xC1: [POP BC](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=38)
    ///
    /// Pops to the 16-bit register BC, data from the stack memory (3 machine cycles).
    fn op_pop_bc(&mut self) {
        self.registers.register_c = self.stack_pop_u8();
        self.registers.register_b = self.stack_pop_u8();
    }

    /// Opcode 0xC2: [JP NZ,a16](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=106)
    ///
    /// Conditional jump to the absolute address specified by the 16-bit operand
    /// following the opcode, depending on the condition NZ. Note that the operand
    /// (absolute address) is read even when the condition is false (3/4 machine
    /// cycles).
    fn op_jp_nz_a16(&mut self) {
        let address = self.fetch_u16();

        if (self.status_flags & STATUS_FLAG_Z) == 0 {
            // TODO: add extra cycle
            self.program_counter = address;
        }
    }

    /// Opcode 0xC3: [JP a16](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=104)
    ///
    /// Unconditional jump to the absolute address specified by the 16-bit immediate
    /// operand following the opcode (4 machine cycles).
    fn op_jp_a16(&mut self) {
        // TODO: need extra cycle
        self.program_counter = self.fetch_u16();
    }

    /// Opcode 0xC4: [CALL NZ,a16](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=112)
    ///
    /// Conditional function call to the absolute address specified by the 16-bit
    /// operand following the opcode, depending on the condition NZ. Note that the
    /// operand (absolute address) is read even when the condition is false (3/6
    /// machine cycles).
    fn op_call_nz_a16(&mut self) {
        let address = self.fetch_u16();

        if (self.status_flags & STATUS_FLAG_Z) == 0 {
            // TODO: need extra cycle
            self.stack_push_u16(self.program_counter);
            self.program_counter = address;
        }
    }

    /// Opcode 0xC5: [PUSH BC](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=37)
    ///
    /// Push to the stack memory, data from the 16-bit register BC (4 machine cycles).
    fn op_push_bc(&mut self) {
        self.stack_push_u8(self.registers.register_b);
        self.stack_push_u8(self.registers.register_c);
    }

    /// Opcode 0xC6: [ADD A,d8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=42)
    ///
    /// Adds to the 8-bit A register, the immediate data following the opcode, and
    /// stores the result back into the A register (2 machine cycles).
    fn op_add_a_u8(&mut self) {
        let operand = self.fetch_u8();
        self.run_add_u8_and_update_flags(operand);
    }

    /// Opcode 0xC7: [RST 00H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=117)
    ///
    /// Unconditional function call to the address 0x0000 (4 machine cycles).
    fn op_rst_00h(&mut self) {
        self.stack_push_u16(self.program_counter);
        self.program_counter = 0x0000;
    }

    /// Opcode 0xC8: [RET Z](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=115)
    ///
    /// Conditional return from a function, depending on the condition 2 (2/5 machine
    /// cycles).
    fn op_ret_z(&mut self) {
        // TODO: extra dummy cycle
        if (self.status_flags & STATUS_FLAG_Z) != 0 {
            // TODO: extra dummy cycle
            self.program_counter = self.stack_pop_u16();
        }
    }

    /// Opcode 0xC9: [RET](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=114)
    ///
    /// Unconditional return from a function (4 machine cycles).
    fn op_ret(&mut self) {
        // TODO: extra cycle
        self.program_counter = self.stack_pop_u16();
    }

    /// Opcode 0xCA: [JP Z,a16](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=106)
    ///
    /// Conditional jump to the absolute address specified by the 16-bit operand
    /// following the opcode, depending on the condition Z. Note that the operand
    /// (absolute address) is read even when the condition is false (3/4 machine
    /// cycles).
    fn op_jp_z_a16(&mut self) {
        let address = self.fetch_u16();

        if (self.status_flags & STATUS_FLAG_Z) != 0 {
            // TODO: add extra cycle
            self.program_counter = address;
        }
    }

    /// Opcode 0xCB: [PREFIX CB]
    ///
    /// Perform a CB prefixed opcode (2/3/4 machine cycles).
    fn op_prefix_cb(&mut self) {
        let cb_code = self.fetch_u8();
        CB_CODE_FUNCTION_TABLE[cb_code as usize](self);
    }

    /// Opcode 0xCC: [CALL Z,a16](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=112)
    ///
    /// Conditional function call to the absolute address specified by the 16-bit
    /// operand following the opcode, depending on the condition Z. Note that the
    /// operand (absolute address) is read even when the condition is false (3/6
    /// machine cycles).
    fn op_call_z_a16(&mut self) {
        let address = self.fetch_u16();

        if (self.status_flags & STATUS_FLAG_Z) != 0 {
            // TODO: need extra cycle
            self.stack_push_u16(self.program_counter);
            self.program_counter = address;
        }
    }

    /// Opcode 0xCD: [CALL a16](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=111)
    ///
    /// Unconditional function call to the absolute address specified by the 16-bit
    /// operand following the opcode (6 machine cycles).
    fn op_call_a16(&mut self) {
        let address = self.fetch_u16();
        self.stack_push_u16(self.program_counter);
        self.program_counter = address;
    }

    /// Opcode 0xCE: [ADC A,d8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=45)
    ///
    /// Adds to the 8-bit A register, the carry flag and the immediate data following
    /// the opcode, and stores the result back into the A register (2 machine cycles).
    fn op_adc_a_u8(&mut self) {
        let operand = self.fetch_u8();
        self.run_adc_and_update_flags(operand);
    }

    /// Opcode 0xCF: [RST 08H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=117)
    ///
    /// Unconditional function call to the address 0x0008 (4 machine cycles).
    fn op_rst_08h(&mut self) {
        self.stack_push_u16(self.program_counter);
        self.program_counter = 0x0008;
    }

    /// Opcode 0xD0: [RET NC](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=115)
    ///
    /// Conditional return from a function, depending on the condition NC (2/5 machine
    /// cycles).
    fn op_ret_nc(&mut self) {
        // TODO: extra dummy cycle
        if (self.status_flags & STATUS_FLAG_C) == 0 {
            // TODO: extra dummy cycle
            self.program_counter = self.stack_pop_u16();
        }
    }

    /// Opcode 0xD1: [POP DE](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=38)
    ///
    /// Pops to the 16-bit register DE, data from the stack memory (3 machine cycles).
    fn op_pop_de(&mut self) {
        self.registers.register_e = self.stack_pop_u8();
        self.registers.register_d = self.stack_pop_u8();
    }

    /// Opcode 0xD2: [JP NC,a16](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=106)
    ///
    /// Conditional jump to the absolute address specified by the 16-bit operand
    /// following the opcode, depending on the condition NC. Note that the operand
    /// (absolute address) is read even when the condition is false (3/4 machine
    /// cycles).
    fn op_jp_nc_a16(&mut self) {
        let address = self.fetch_u16();

        if (self.status_flags & STATUS_FLAG_C) == 0 {
            // TODO: add extra cycle
            self.program_counter = address;
        }
    }

    /// Opcode 0xD4: [CALL NC,a16](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=112)
    ///
    /// Conditional function call to the absolute address specified by the 16-bit
    /// operand following the opcode, depending on the condition NC. Note that the
    /// operand (absolute address) is read even when the condition is false (3/6
    /// machine cycles).
    fn op_call_nc_a16(&mut self) {
        let address = self.fetch_u16();

        if (self.status_flags & STATUS_FLAG_C) == 0 {
            // TODO: need extra cycle
            self.stack_push_u16(self.program_counter);
            self.program_counter = address;
        }
    }

    /// Opcode 0xD5: [PUSH DE](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=37)
    ///
    /// Push to the stack memory, data from the 16-bit register DE (4 machine cycles).
    fn op_push_de(&mut self) {
        self.stack_push_u8(self.registers.register_d);
        self.stack_push_u8(self.registers.register_e);
    }

    /// Opcode 0xD6: [SUB d8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=48)
    ///
    /// Subtracts from the 8-bit A register, the immediate data following the opcode,
    /// and stores the result back into the A register (2 machine cycles).
    fn op_sub_a_u8(&mut self) {
        let operand = self.fetch_u8();
        self.run_sub_and_update_flags(operand);
    }

    /// Opcode 0xD7: [RST 10H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=117)
    ///
    /// Unconditional function call to the address 0x0010 (4 machine cycles).
    fn op_rst_10h(&mut self) {
        self.stack_push_u16(self.program_counter);
        self.program_counter = 0x0010;
    }

    /// Opcode 0xD8: [RET C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=115)
    ///
    /// Conditional return from a function, depending on the condition C (2/5 machine
    /// cycles).
    fn op_ret_c(&mut self) {
        // TODO: extra dummy cycle
        if (self.status_flags & STATUS_FLAG_C) != 0 {
            // TODO: extra dummy cycle
            self.program_counter = self.stack_pop_u16();
        }
    }

    /// Opcode 0xD9: [RETI](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=116)
    ///
    /// Unconditional return from a function. Also enables interrupts by setting IME=1
    /// (4 machine cycles).
    fn op_reti(&mut self) {
        // TODO: extra dummy cycle
        self.program_counter = self.stack_pop_u16();
        self.interrupt_master_enabled = true;
    }

    /// Opcode 0xDA: [JP C,a16](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=106)
    ///
    /// Conditional jump to the absolute address specified by the 16-bit operand
    /// following the opcode, depending on the condition C. Note that the operand
    /// (absolute address) is read even when the condition is false (3/4 machine
    /// cycles).
    fn op_jp_c_a16(&mut self) {
        let address = self.fetch_u16();

        if (self.status_flags & STATUS_FLAG_C) != 0 {
            // TODO: add extra cycle
            self.program_counter = address;
        }
    }

    /// Opcode 0xDC: [CALL C,a16](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=112)
    ///
    /// Conditional function call to the absolute address specified by the 16-bit
    /// operand following the opcode, depending on the condition C. Note that the
    /// operand (absolute address) is read even when the condition is false (3/6
    /// machine cycles).
    fn op_call_c_a16(&mut self) {
        let address = self.fetch_u16();

        if (self.status_flags & STATUS_FLAG_C) != 0 {
            // TODO: need extra cycle
            self.stack_push_u16(self.program_counter);
            self.program_counter = address;
        }
    }

    /// Opcode 0xDE: [SBC A,d8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=51)
    ///
    /// Subtracts from the 8-bit A register, the carry flag and the immediate data
    /// following the opcode, and stores the result back into the A register (2 machine
    /// cycles).
    fn op_sbc_a_u8(&mut self) {
        let operand = self.fetch_u8();
        self.run_sbc_and_update_flags(operand);
    }

    /// Opcode 0xDF: [RST 18H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=117)
    ///
    /// Unconditional function call to the address 0x0018 (4 machine cycles).
    fn op_rst_18h(&mut self) {
        self.stack_push_u16(self.program_counter);
        self.program_counter = 0x0018;
    }

    /// Opcode 0xE0: [LDH (a8),A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=29)
    ///
    /// Load to the address specified by the 8-bit immediate data following the opcode,
    /// data from the 8-bit A register. The full 16-bit absolute address is obtained by
    /// setting the most significant byte to 0xFF and the least significant byte to the
    /// value of n, so the possible range is 0xFF00-0xFFFF (3 machine cycles).
    fn op_ldh_a8_a(&mut self) {
        let address = 0xFF00 | (self.fetch_u8() as u16);
        self.memory.write(address, self.registers.register_a);
    }

    /// Opcode 0xE1: [POP HL](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=38)
    ///
    /// Pops to the 16-bit register HL, data from the stack memory (3 machine cycles).
    fn op_pop_hl(&mut self) {
        self.registers.register_l = self.stack_pop_u8();
        self.registers.register_h = self.stack_pop_u8();
    }

    /// Opcode 0xE2: [LDH (C),A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=38)
    ///
    /// Load to the address specified by the 8-bit C register, data from the 8-bit A
    /// register. The full 16-bit absolute address is obtained by setting the most
    /// significant byte to 0xFF and the least significant byte to the value of C, so
    /// the possible range is 0xFF00-0xFFFF (2 machine cycles).
    fn ldh_c_a(&mut self) {
        let address = 0xFF00 & self.registers.register_c as u16;
        self.memory.write(address, self.registers.register_a);
    }

    /// Opcode 0xE5: [PUSH HL](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=37)
    ///
    /// Push to the stack memory, data from the 16-bit register HL (4 machine cycles).
    fn op_push_hl(&mut self) {
        self.stack_push_u8(self.registers.register_h);
        self.stack_push_u8(self.registers.register_l);
    }

    /// Opcode 0xE6: [AND d8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=61)
    ///
    /// Performs a bitwise AND operation between the 8-bit A register and immediate
    /// data following the opcode, and stores the result back into the A register (2
    /// machine cycles).
    fn op_and_a_u8(&mut self) {
        let operand = self.fetch_u8();
        self.run_and_and_update_flags(operand);
    }

    /// Opcode 0xE7: [RST 20H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=117)
    ///
    /// Unconditional function call to the address 0x0020 (4 machine cycles).
    fn op_rst_20h(&mut self) {
        self.stack_push_u16(self.program_counter);
        self.program_counter = 0x0020;
    }

    /// Opcode 0xE8: [ADD SP,r8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=75)
    ///
    /// Loads to the 16-bit SP register, 16-bit data calculated by adding the signed
    /// 8-bit operand following the opcode to the 16-bit value of the SP register (4
    /// machine cycles).
    fn op_add_sp_r8(&mut self) {
        let offset = self.fetch_u8() as i8;

        // TODO: This op is supposed to be 4 machine cycles long, needs 2 extra dummy cycles
        self.stack_pointer = (self.stack_pointer as i32).wrapping_add(offset as i32) as u16;
        self.status_flags = 0;

        // TODO: check type convertion...
        if ((self.stack_pointer & 0x0F) + (offset as u16 & 0x0F)) > 0x0F {
            self.status_flags |= STATUS_FLAG_H;
        }

        if ((self.stack_pointer & 0xFF) + (offset as u16 & 0xFF)) > 0xFF {
            self.status_flags |= STATUS_FLAG_C;
        }
    }

    /// Opcode 0xE9: [JP (HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=105)
    ///
    /// Unconditional jump to the absolute address specified by the 16-bit register HL
    /// (1 machine cycles).
    fn op_jp_hl(&mut self) {
        self.program_counter = self.registers.hl();
    }

    /// Opcode 0xEA: [LD (a16),A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=25)
    ///
    /// Load to the absolute address specified by the 16-bit operand following the
    /// opcode, data from the 8-bit A register (4 machine cycles).
    fn op_ld_a16_a(&mut self) {
        let address = self.fetch_u16();
        self.memory.write(address, self.registers.register_a);
    }

    /// Opcode 0xEE: [XOR d8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=67)
    ///
    /// Performs a bitwise XOR operation between the 8-bit A register and immediate
    /// data following the opcode, and stores the result back into the A register (4
    /// machine cycles).
    fn op_xor_a_u8(&mut self) {
        let operand = self.fetch_u8();
        self.run_xor_and_update_flags(operand);
    }

    /// Opcode 0xEF: [RST 28H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=117)
    ///
    /// Unconditional function call to the address 0x0028 (4 machine cycles).
    fn op_rst_28h(&mut self) {
        self.stack_push_u16(self.program_counter);
        self.program_counter = 0x0028;
    }

    /// Opcode 0xF0: [LDH A,(a8)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=28)
    ///
    /// Load to the 8-bit A register, data from the address specified by the 8-bit
    /// immediate data following the opcode. The full 16-bit absolute address is
    /// obtained by setting the most significant byte to 0xFF and the least significant
    /// byte to the value of n, so the possible range is 0xFF00-0xFFFF (3 machine
    /// cycles).
    fn op_ldh_a_a8(&mut self) {
        let address = 0xFF00 | (self.fetch_u8() as u16);
        self.registers.register_a = self.memory.read(address);
    }

    /// Opcode 0xF1: [POP AF](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=38)
    ///
    /// Pops to the 16-bit register AF, data from the stack memory. This instruction
    /// does not do calculations that affect flags, but POP AF completely replaces the
    /// F register value, so all flags are changed based on the 8-bit data that is read
    /// from memory (3 machine cycles).
    fn op_pop_af(&mut self) {
        self.status_flags = self.stack_pop_u8();
        self.registers.register_a = self.stack_pop_u8();
    }

    /// Opcode 0xE2: [LDH A,(C)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=38)
    ///
    /// Load to the 8-bit A register, data from the address specified by the 8-bit C
    /// register. The full 16-bit absolute address is obtained by setting the most
    /// significant byte to 0xFF and the least significant byte to the value of C, so
    /// the possible range is 0xFF00-0xFFFF (2 machine cycles).
    fn ldh_a_c(&mut self) {
        let address = 0xFF00 & self.registers.register_c as u16;
        self.registers.register_a = self.memory.read(address);
    }

    /// Opcode 0xF3: [DI](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=118)
    ///
    /// Disables interrupt handling by setting IME=0 and cancelling any scheduled
    /// effects of the EI instruction if any (1 machine cycles).
    fn op_di(&mut self) {
        self.interrupt_master_enabled = false;
        // TODO cancel effects of EI
    }

    /// Opcode 0xF5: [PUSH AF](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=37)
    ///
    /// Push to the stack memory, data from the 16-bit register AF (4 machine cycles).
    fn op_push_af(&mut self) {
        self.stack_push_u8(self.registers.register_a);
        self.stack_push_u8(self.status_flags);
    }

    /// Opcode 0xF6: [OR d8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=64)
    ///
    /// Performs a bitwise OR operation between the 8-bit A register and immediate data
    /// following the opcode, and stores the result back into the A register (2 machine
    /// cycles).
    fn op_or_a_u8(&mut self) {
        let operand = self.fetch_u8();
        self.run_or_and_update_flags(operand);
    }

    /// Opcode 0xF7: [RST 30H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=117)
    ///
    /// Unconditional function call to the address 0x0030 (4 machine cycles).
    fn op_rst_30h(&mut self) {
        self.stack_push_u16(self.program_counter);
        self.program_counter = 0x0030;
    }

    /// Opcode 0xF8: [LD HL,SP+r8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=39)
    ///
    /// Load to the HL register, 16-bit data calculated by adding the signed 8-bit
    /// operand e to the 16-bit value of the SP register (3 machine cycles).
    fn op_ld_hl_sp_i8(&mut self) {
        let offset = self.fetch_u8() as i8;

        self.status_flags = 0;
        self.registers
            .set_hl((self.stack_pointer as i32).wrapping_add(offset as i32) as u16);

        // TODO: check type convertion...
        if ((self.stack_pointer & 0x0F) + (offset as u16 & 0x0F)) > 0x0F {
            self.status_flags |= STATUS_FLAG_H;
        }

        if ((self.stack_pointer & 0xFF) + (offset as u16 & 0xFF)) > 0xFF {
            self.status_flags |= STATUS_FLAG_C;
        }
    }

    /// Opcode 0xF9: [LD SP,HL](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=36)
    ///
    /// Load to the 16-bit SP register, data from the 16-bit HL register (2 machine
    /// cycles).
    fn op_ld_sp_hl(&mut self) {
        self.stack_pointer = self.registers.hl();
    }

    /// Opcode 0xFA: [LD A,(a16)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=24)
    ///
    /// Load to the 8-bit A register, data from the absolute address specified by the
    /// 16-bit operand following the opcode (4 machine cycles).
    fn op_ld_a_a16(&mut self) {
        let address = self.fetch_u16();
        self.registers.register_a = self.memory.read(address);
    }

    /// Opcode 0xFB: [EI](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=119)
    ///
    /// Schedules interrupt handling to be enabled after the next machine cycle (1
    /// machine cycle).
    fn op_ei(&mut self) {
        // TODO: should be delayed by one cycle?
        self.interrupt_master_enabled = false;
    }

    /// Opcode 0xFE: [CP d8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=54)
    ///
    /// Subtracts from the 8-bit A register, the immediate data n, and updates flags
    /// based on the result. This instruction is basically identical to SUB d8, but
    /// does not update the A register (2 machine cycles).
    fn op_cp_a_u8(&mut self) {
        let operand = self.fetch_u8();
        self.run_cp_and_update_flags(operand);
    }

    /// Opcode 0xFF: [RST 38H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=117)
    ///
    /// Unconditional function call to the address 0x0038 (4 machine cycles).
    fn op_rst_38h(&mut self) {
        self.stack_push_u16(self.program_counter);
        self.program_counter = 0x0038;
    }
}

impl Cpu {
    /// Opcode 0x00: [RLC B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=81)
    ///
    /// Circular rotate left of the 8-bit register B (2 machine cycles).
    fn cb_rlc_b(&mut self) {
        self.registers.register_b = self.run_rlc_u8_and_update_flags(self.registers.register_b);
    }

    /// Opcode 0x01: [RLC C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=81)
    ///
    /// Circular rotate left of the 8-bit register C (2 machine cycles).
    fn cb_rlc_c(&mut self) {
        self.registers.register_c = self.run_rlc_u8_and_update_flags(self.registers.register_c);
    }

    /// Opcode 0x02: [RLC D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=81)
    ///
    /// Circular rotate left of the 8-bit register D (2 machine cycles).
    fn cb_rlc_d(&mut self) {
        self.registers.register_d = self.run_rlc_u8_and_update_flags(self.registers.register_d);
    }

    /// Opcode 0x03: [RLC E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=81)
    ///
    /// Circular rotate left of the 8-bit register E (2 machine cycles).
    fn cb_rlc_e(&mut self) {
        self.registers.register_e = self.run_rlc_u8_and_update_flags(self.registers.register_e);
    }

    /// Opcode 0x04: [RLC H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=81)
    ///
    /// Circular rotate left of the 8-bit register H (2 machine cycles).
    fn cb_rlc_h(&mut self) {
        self.registers.register_h = self.run_rlc_u8_and_update_flags(self.registers.register_h);
    }

    /// Opcode 0x05: [RLC L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=81)
    ///
    /// Circular rotate left of the 8-bit register L (2 machine cycles).
    fn cb_rlc_l(&mut self) {
        self.registers.register_l = self.run_rlc_u8_and_update_flags(self.registers.register_l);
    }

    /// Opcode 0x06: [RLC (HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=82)
    ///
    /// Circular rotate left of data from the absolute address specified by the 16-bit
    /// register HL (4 machine cycles).
    fn cb_rlc_hl(&mut self) {
        let operand = self.read_hl();
        let value = self.run_rlc_u8_and_update_flags(operand);
        self.write_hl(value);
    }

    /// Opcode 0x07: [RLC A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=81)
    ///
    /// Circular rotate left of the 8-bit register A (2 machine cycles).
    fn cb_rlc_a(&mut self) {
        self.registers.register_a = self.run_rlc_u8_and_update_flags(self.registers.register_a);
    }

    /// Opcode 0x08: [RRC B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=83)
    ///
    /// Circular rotate right of the 8-bit register B (2 machine cycles).
    fn cb_rrc_b(&mut self) {
        self.registers.register_b = self.run_rrc_u8_and_update_flags(self.registers.register_b);
    }

    /// Opcode 0x09: [RRC C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=83)
    ///
    /// Circular rotate right of the 8-bit register C (2 machine cycles).
    fn cb_rrc_c(&mut self) {
        self.registers.register_c = self.run_rrc_u8_and_update_flags(self.registers.register_c);
    }

    /// Opcode 0x0A: [RRC D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=83)
    ///
    /// Circular rotate right of the 8-bit register D (2 machine cycles).
    fn cb_rrc_d(&mut self) {
        self.registers.register_d = self.run_rrc_u8_and_update_flags(self.registers.register_d);
    }

    /// Opcode 0x0B: [RRC E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=83)
    ///
    /// Circular rotate right of the 8-bit register E (2 machine cycles).
    fn cb_rrc_e(&mut self) {
        self.registers.register_e = self.run_rrc_u8_and_update_flags(self.registers.register_e);
    }

    /// Opcode 0x0C: [RRC H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=83)
    ///
    /// Circular rotate right of the 8-bit register H (2 machine cycles).
    fn cb_rrc_h(&mut self) {
        self.registers.register_h = self.run_rrc_u8_and_update_flags(self.registers.register_h);
    }

    /// Opcode 0x0D: [RRC L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=83)
    ///
    /// Circular rotate right of the 8-bit register L (2 machine cycles).
    fn cb_rrc_l(&mut self) {
        self.registers.register_l = self.run_rrc_u8_and_update_flags(self.registers.register_l);
    }

    /// Opcode 0x0E: [RRC (HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=84)
    ///
    /// Circular rotate right of data from the absolute address specified by the 16-bit
    /// register HL (4 machine cycles).
    fn cb_rrc_hl(&mut self) {
        let operand = self.read_hl();
        let value = self.run_rrc_u8_and_update_flags(operand);
        self.write_hl(value);
    }

    /// Opcode 0x0F: [RRC A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=83)
    ///
    /// Circular rotate right of the 8-bit register A (2 machine cycles).
    fn cb_rrc_a(&mut self) {
        self.registers.register_a = self.run_rrc_u8_and_update_flags(self.registers.register_a);
    }

    /// Opcode 0x10: [RL B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=85)
    ///
    /// Rotate left of the 8-bit register B (2 machine cycles).
    fn cb_rl_b(&mut self) {
        self.registers.register_b = self.run_rl_u8_and_update_flags(self.registers.register_b);
    }

    /// Opcode 0x11: [RL C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=85)
    ///
    /// Rotate left of the 8-bit register C (2 machine cycles).
    fn cb_rl_c(&mut self) {
        self.registers.register_c = self.run_rl_u8_and_update_flags(self.registers.register_c);
    }

    /// Opcode 0x12: [RL D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=85)
    ///
    /// Rotate left of the 8-bit register D (2 machine cycles).
    fn cb_rl_d(&mut self) {
        self.registers.register_d = self.run_rl_u8_and_update_flags(self.registers.register_d);
    }

    /// Opcode 0x13: [RL E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=85)
    ///
    /// Rotate left of the 8-bit register E (2 machine cycles).
    fn cb_rl_e(&mut self) {
        self.registers.register_e = self.run_rl_u8_and_update_flags(self.registers.register_e);
    }

    /// Opcode 0x14: [RL H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=85)
    ///
    /// Rotate left of the 8-bit register H (2 machine cycles).
    fn cb_rl_h(&mut self) {
        self.registers.register_h = self.run_rl_u8_and_update_flags(self.registers.register_h);
    }

    /// Opcode 0x15: [RL L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=85)
    ///
    /// Rotate left of the 8-bit register L (2 machine cycles).
    fn cb_rl_l(&mut self) {
        self.registers.register_l = self.run_rl_u8_and_update_flags(self.registers.register_l);
    }

    /// Opcode 0x16: [RL (HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=86)
    ///
    /// Rotate left of data from the absolute address specified by the 16-bit register
    /// HL (4 machine cycles).
    fn cb_rl_hl(&mut self) {
        let operand = self.read_hl();
        let value = self.run_rl_u8_and_update_flags(operand);
        self.write_hl(value);
    }

    /// Opcode 0x17: [RL A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=85)
    ///
    /// Rotate left of the 8-bit register A (2 machine cycles).
    fn cb_rl_a(&mut self) {
        self.registers.register_a = self.run_rl_u8_and_update_flags(self.registers.register_a);
    }

    /// Opcode 0x18: [RR B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=87)
    ///
    /// Rotate right of the 8-bit register B (2 machine cycles).
    fn cb_rr_b(&mut self) {
        self.registers.register_b = self.run_rr_u8_and_update_flags(self.registers.register_b);
    }

    /// Opcode 0x19: [RR C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=87)
    ///
    /// Rotate right of the 8-bit register C (2 machine cycles).
    fn cb_rr_c(&mut self) {
        self.registers.register_c = self.run_rr_u8_and_update_flags(self.registers.register_c);
    }

    /// Opcode 0x1A: [RR D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=87)
    ///
    /// Rotate right of the 8-bit register D (2 machine cycles).
    fn cb_rr_d(&mut self) {
        self.registers.register_d = self.run_rr_u8_and_update_flags(self.registers.register_d);
    }

    /// Opcode 0x1B: [RR E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=87)
    ///
    /// Rotate right of the 8-bit register E (2 machine cycles).
    fn cb_rr_e(&mut self) {
        self.registers.register_e = self.run_rr_u8_and_update_flags(self.registers.register_e);
    }

    /// Opcode 0x1C: [RR H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=87)
    ///
    /// Rotate right of the 8-bit register H (2 machine cycles).
    fn cb_rr_h(&mut self) {
        self.registers.register_h = self.run_rr_u8_and_update_flags(self.registers.register_h);
    }

    /// Opcode 0x1D: [RR L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=87)
    ///
    /// Rotate right of the 8-bit register L (2 machine cycles).
    fn cb_rr_l(&mut self) {
        self.registers.register_l = self.run_rr_u8_and_update_flags(self.registers.register_l);
    }

    /// Opcode 0x1E: [RR (HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=88)
    ///
    /// Rotate right of data from the absolute address specified by the 16-bit
    /// register HL (4 machine cycles).
    fn cb_rr_hl(&mut self) {
        let operand = self.read_hl();
        let value = self.run_rr_u8_and_update_flags(operand);
        self.write_hl(value);
    }

    /// Opcode 0x1F: [RR A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=87)
    ///
    /// Rotate right of the 8-bit register A (2 machine cycles).
    fn cb_rr_a(&mut self) {
        self.registers.register_a = self.run_rr_u8_and_update_flags(self.registers.register_a);
    }

    /// Opcode 0x20: [SLA B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=89)
    ///
    /// Shift left of the 8-bit register B (2 machine cycles).
    fn cb_sla_b(&mut self) {
        self.registers.register_b = self.run_sla_u8_and_update_flags(self.registers.register_b);
    }

    /// Opcode 0x21: [SLA C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=89)
    ///
    /// Shift left of the 8-bit register C (2 machine cycles).
    fn cb_sla_c(&mut self) {
        self.registers.register_c = self.run_sla_u8_and_update_flags(self.registers.register_c);
    }

    /// Opcode 0x22: [SLA D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=89)
    ///
    /// Shift left of the 8-bit register D (2 machine cycles).
    fn cb_sla_d(&mut self) {
        self.registers.register_d = self.run_sla_u8_and_update_flags(self.registers.register_d);
    }

    /// Opcode 0x23: [SLA E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=89)
    ///
    /// Shift left of the 8-bit register E (2 machine cycles).
    fn cb_sla_e(&mut self) {
        self.registers.register_e = self.run_sla_u8_and_update_flags(self.registers.register_e);
    }

    /// Opcode 0x24: [SLA H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=89)
    ///
    /// Rotate left of the 8-bit register H (2 machine cycles).
    fn cb_sla_h(&mut self) {
        self.registers.register_h = self.run_sla_u8_and_update_flags(self.registers.register_h);
    }

    /// Opcode 0x25: [SLA L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=89)
    ///
    /// Rotate left of the 8-bit register L (2 machine cycles).
    fn cb_sla_l(&mut self) {
        self.registers.register_l = self.run_sla_u8_and_update_flags(self.registers.register_l);
    }

    /// Opcode 0x26: [SLA (HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=90)
    ///
    /// Shift left of data from the absolute address specified by the 16-bit register
    /// HL (4 machine cycles).
    fn cb_sla_hl(&mut self) {
        let operand = self.read_hl();
        let value = self.run_sla_u8_and_update_flags(operand);
        self.write_hl(value);
    }

    /// Opcode 0x27: [SLA A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=89)
    ///
    /// Shift left of the 8-bit register A (2 machine cycles).
    fn cb_sla_a(&mut self) {
        self.registers.register_a = self.run_sla_u8_and_update_flags(self.registers.register_a);
    }

    /// Opcode 0x28: [RR B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=91)
    ///
    /// Shift right of the 8-bit register B (2 machine cycles).
    fn cb_sra_b(&mut self) {
        self.registers.register_b = self.run_sra_u8_and_update_flags(self.registers.register_b);
    }

    /// Opcode 0x29: [RR C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=91)
    ///
    /// Shift right of the 8-bit register C (2 machine cycles).
    fn cb_sra_c(&mut self) {
        self.registers.register_c = self.run_sra_u8_and_update_flags(self.registers.register_c);
    }

    /// Opcode 0x2A: [RR D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=91)
    ///
    /// Shift right of the 8-bit register D (2 machine cycles).
    fn cb_sra_d(&mut self) {
        self.registers.register_d = self.run_sra_u8_and_update_flags(self.registers.register_d);
    }

    /// Opcode 0x2B: [RR E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=91)
    ///
    /// Shift right of the 8-bit register E (2 machine cycles).
    fn cb_sra_e(&mut self) {
        self.registers.register_e = self.run_sra_u8_and_update_flags(self.registers.register_e);
    }

    /// Opcode 0x2C: [RR H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=91)
    ///
    /// Shift right of the 8-bit register H (2 machine cycles).
    fn cb_sra_h(&mut self) {
        self.registers.register_h = self.run_sra_u8_and_update_flags(self.registers.register_h);
    }

    /// Opcode 0x2D: [RR L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=91)
    ///
    /// Shift right of the 8-bit register L (2 machine cycles).
    fn cb_sra_l(&mut self) {
        self.registers.register_l = self.run_sra_u8_and_update_flags(self.registers.register_l);
    }

    /// Opcode 0x2E: [RR (HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=92)
    ///
    /// Shift right of data from the absolute address specified by the 16-bit register
    /// HL (4 machine cycles).
    fn cb_sra_hl(&mut self) {
        let operand = self.read_hl();
        let value = self.run_sra_u8_and_update_flags(operand);
        self.write_hl(value);
    }

    /// Opcode 0x2F: [RR A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=91)
    ///
    /// Shift right of the 8-bit register A (2 machine cycles).
    fn cb_sra_a(&mut self) {
        self.registers.register_a = self.run_sra_u8_and_update_flags(self.registers.register_a);
    }

    /// Opcode 0x30: [SWAP B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=93)
    ///
    /// Swap the two halves of the 8-bit register B (2 machine cycles).
    fn cb_swap_b(&mut self) {
        self.registers.register_b = self.run_swap_u8_and_update_flags(self.registers.register_b);
    }

    /// Opcode 0x31: [SWAP C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=93)
    ///
    /// Swap the two halves of the 8-bit register C (2 machine cycles).
    fn cb_swap_c(&mut self) {
        self.registers.register_c = self.run_swap_u8_and_update_flags(self.registers.register_c);
    }

    /// Opcode 0x32: [SWAP D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=93)
    ///
    /// Swap the two halves of the 8-bit register D (2 machine cycles).
    fn cb_swap_d(&mut self) {
        self.registers.register_d = self.run_swap_u8_and_update_flags(self.registers.register_d);
    }

    /// Opcode 0x33: [SWAP E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=93)
    ///
    /// Swap the two halves of the 8-bit register E (2 machine cycles).
    fn cb_swap_e(&mut self) {
        self.registers.register_e = self.run_swap_u8_and_update_flags(self.registers.register_e);
    }

    /// Opcode 0x34: [SWAP H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=93)
    ///
    /// Swap the two halves of the 8-bit register H (2 machine cycles).
    fn cb_swap_h(&mut self) {
        self.registers.register_h = self.run_swap_u8_and_update_flags(self.registers.register_h);
    }

    /// Opcode 0x35: [SWAP L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=93)
    ///
    /// Swap the two halves of the 8-bit register L (2 machine cycles).
    fn cb_swap_l(&mut self) {
        self.registers.register_l = self.run_swap_u8_and_update_flags(self.registers.register_l);
    }

    /// Opcode 0x36: [SWAP (HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=94)
    ///
    /// Swap the two halves of the data at the absolute address specified by the 16-bit
    /// register HL (4 machine cycles).
    fn cb_swap_hl(&mut self) {
        let operand = self.read_hl();
        let value = self.run_swap_u8_and_update_flags(operand);
        self.write_hl(value);
    }

    /// Opcode 0x37: [SWAP A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=93)
    ///
    /// Swap the two halves of the 8-bit register A (2 machine cycles).
    fn cb_swap_a(&mut self) {
        self.registers.register_a = self.run_swap_u8_and_update_flags(self.registers.register_a);
    }

    /// Opcode 0x38: [SRL B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=95)
    ///
    /// Shift right of the 8-bit register B (2 machine cycles).
    fn cb_srl_b(&mut self) {
        self.registers.register_b = self.run_srl_u8_and_update_flags(self.registers.register_b);
    }

    /// Opcode 0x39: [SRL C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=95)
    ///
    /// Shift right of the 8-bit register C (2 machine cycles).
    fn cb_srl_c(&mut self) {
        self.registers.register_c = self.run_srl_u8_and_update_flags(self.registers.register_c);
    }

    /// Opcode 0x3A: [SRL D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=95)
    ///
    /// Shift right of the 8-bit register D (2 machine cycles).
    fn cb_srl_d(&mut self) {
        self.registers.register_d = self.run_srl_u8_and_update_flags(self.registers.register_d);
    }

    /// Opcode 0x3B: [SRL E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=95)
    ///
    /// Shift right of the 8-bit register E (2 machine cycles).
    fn cb_srl_e(&mut self) {
        self.registers.register_e = self.run_srl_u8_and_update_flags(self.registers.register_e);
    }

    /// Opcode 0x3C: [SRL H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=95)
    ///
    /// Shift right of the 8-bit register H (2 machine cycles).
    fn cb_srl_h(&mut self) {
        self.registers.register_h = self.run_srl_u8_and_update_flags(self.registers.register_h);
    }

    /// Opcode 0x3D: [SRL L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=95)
    ///
    /// Shift right of the 8-bit register L (2 machine cycles).
    fn cb_srl_l(&mut self) {
        self.registers.register_l = self.run_srl_u8_and_update_flags(self.registers.register_l);
    }

    /// Opcode 0x3E: [SRL (HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=96)
    ///
    /// Shift right of data from the absolute address specified by the 16-bit register
    /// HL (4 machine cycles).
    fn cb_srl_hl(&mut self) {
        let operand = self.read_hl();
        let value = self.run_srl_u8_and_update_flags(operand);
        self.write_hl(value);
    }

    /// Opcode 0x3F: [SRL A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=95)
    ///
    /// Shift right of the 8-bit register A (2 machine cycles).
    fn cb_srl_a(&mut self) {
        self.registers.register_a = self.run_srl_u8_and_update_flags(self.registers.register_a);
    }

    /// Opcode 0x40: [BIT 0,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 0 of 8-bit register B is 0 (2 machine cycles).
    fn cb_bit_0_b(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_b, 0x01);
    }

    /// Opcode 0x41: [BIT 0,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 0 of 8-bit register C is 0 (2 machine cycles).
    fn cb_bit_0_c(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_c, 0x01);
    }

    /// Opcode 0x42: [BIT 0,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 0 of 8-bit register D is 0 (2 machine cycles).
    fn cb_bit_0_d(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_d, 0x01);
    }

    /// Opcode 0x43: [BIT 0,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 0 of 8-bit register E is 0 (2 machine cycles).
    fn cb_bit_0_e(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_e, 0x01);
    }

    /// Opcode 0x44: [BIT 0,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 0 of 8-bit register H is 0 (2 machine cycles).
    fn cb_bit_0_h(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_h, 0x01);
    }

    /// Opcode 0x45: [BIT 0,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 0 of 8-bit register L is 0 (2 machine cycles).
    fn cb_bit_0_l(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_l, 0x01);
    }

    /// Opcode 0x46: [BIT 0,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=98)
    ///
    /// Test if the bit 0 of the data at the absolute address specified by the 16-bit
    /// register HL is 0 (3 machine cycles).
    fn cb_bit_0_hl(&mut self) {
        let operand = self.read_hl();
        self.run_bit_u8_and_update_flags(operand, 0x01);
    }

    /// Opcode 0x47: [BIT 0,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 0 of 8-bit register 1 is 0 (2 machine cycles).
    fn cb_bit_0_a(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_a, 0x01);
    }

    /// Opcode 0x48: [BIT 1,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 1 of 8-bit register B is 0 (2 machine cycles).
    fn cb_bit_1_b(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_b, 0x02);
    }

    /// Opcode 0x49: [BIT 1,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 1 of 8-bit register C is 0 (2 machine cycles).
    fn cb_bit_1_c(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_c, 0x02);
    }

    /// Opcode 0x4A: [BIT 1,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 1 of 8-bit register D is 0 (2 machine cycles).
    fn cb_bit_1_d(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_d, 0x02);
    }

    /// Opcode 0x4B: [BIT 1,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 1 of 8-bit register E is 0 (2 machine cycles).
    fn cb_bit_1_e(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_e, 0x02);
    }

    /// Opcode 0x4C: [BIT 1,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 1 of 8-bit register H is 0 (2 machine cycles).
    fn cb_bit_1_h(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_h, 0x02);
    }

    /// Opcode 0x4D: [BIT 1,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 1 of 8-bit register L is 0 (2 machine cycles).
    fn cb_bit_1_l(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_l, 0x02);
    }

    /// Opcode 0x4E: [BIT 1,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=98)
    ///
    /// Test if the bit 1 of the data at the absolute address specified by the 16-bit
    /// register HL is 0 (3 machine cycles).
    fn cb_bit_1_hl(&mut self) {
        let operand = self.read_hl();
        self.run_bit_u8_and_update_flags(operand, 0x02);
    }

    /// Opcode 0x4F: [BIT 1,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 1 of 8-bit register 1 is 0 (2 machine cycles).
    fn cb_bit_1_a(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_a, 0x02);
    }

    /// Opcode 0x50: [BIT 2,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 2 of 8-bit register B is 0 (2 machine cycles).
    fn cb_bit_2_b(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_b, 0x04);
    }

    /// Opcode 0x51: [BIT 2,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 2 of 8-bit register C is 0 (2 machine cycles).
    fn cb_bit_2_c(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_c, 0x04);
    }

    /// Opcode 0x52: [BIT 2,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 2 of 8-bit register D is 0 (2 machine cycles).
    fn cb_bit_2_d(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_d, 0x04);
    }

    /// Opcode 0x53: [BIT 2,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 2 of 8-bit register E is 0 (2 machine cycles).
    fn cb_bit_2_e(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_e, 0x04);
    }

    /// Opcode 0x54: [BIT 2,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 2 of 8-bit register H is 0 (2 machine cycles).
    fn cb_bit_2_h(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_h, 0x04);
    }

    /// Opcode 0x55: [BIT 2,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 2 of 8-bit register L is 0 (2 machine cycles).
    fn cb_bit_2_l(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_l, 0x04);
    }

    /// Opcode 0x56: [BIT 2,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=98)
    ///
    /// Test if the bit 2 of the data at the absolute address specified by the 16-bit
    /// register HL is 0 (3 machine cycles).
    fn cb_bit_2_hl(&mut self) {
        let operand = self.read_hl();
        self.run_bit_u8_and_update_flags(operand, 0x04);
    }

    /// Opcode 0x57: [BIT 2,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 2 of 8-bit register 1 is 0 (2 machine cycles).
    fn cb_bit_2_a(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_a, 0x04);
    }

    /// Opcode 0x58: [BIT 3,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 3 of 8-bit register B is 0 (2 machine cycles).
    fn cb_bit_3_b(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_b, 0x08);
    }

    /// Opcode 0x59: [BIT 3,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 3 of 8-bit register C is 0 (2 machine cycles).
    fn cb_bit_3_c(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_c, 0x08);
    }

    /// Opcode 0x5A: [BIT 3,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 3 of 8-bit register D is 0 (2 machine cycles).
    fn cb_bit_3_d(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_d, 0x08);
    }

    /// Opcode 0x5B: [BIT 3,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 3 of 8-bit register E is 0 (2 machine cycles).
    fn cb_bit_3_e(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_e, 0x08);
    }

    /// Opcode 0x5C: [BIT 3,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 3 of 8-bit register H is 0 (2 machine cycles).
    fn cb_bit_3_h(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_h, 0x08);
    }

    /// Opcode 0x5D: [BIT 3,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 3 of 8-bit register L is 0 (2 machine cycles).
    fn cb_bit_3_l(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_l, 0x08);
    }

    /// Opcode 0x5E: [BIT 3,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=98)
    ///
    /// Test if the bit 3 of the data at the absolute address specified by the 16-bit
    /// register HL is 0 (3 machine cycles).
    fn cb_bit_3_hl(&mut self) {
        let operand = self.read_hl();
        self.run_bit_u8_and_update_flags(operand, 0x08);
    }

    /// Opcode 0x5F: [BIT 3,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 3 of 8-bit register 1 is 0 (2 machine cycles).
    fn cb_bit_3_a(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_a, 0x08);
    }

    /// Opcode 0x60: [BIT 4,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 4 of 8-bit register B is 0 (2 machine cycles).
    fn cb_bit_4_b(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_b, 0x10);
    }

    /// Opcode 0x61: [BIT 4,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 4 of 8-bit register C is 0 (2 machine cycles).
    fn cb_bit_4_c(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_c, 0x10);
    }

    /// Opcode 0x62: [BIT 4,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 4 of 8-bit register D is 0 (2 machine cycles).
    fn cb_bit_4_d(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_d, 0x10);
    }

    /// Opcode 0x63: [BIT 4,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 4 of 8-bit register E is 0 (2 machine cycles).
    fn cb_bit_4_e(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_e, 0x10);
    }

    /// Opcode 0x64: [BIT 4,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 4 of 8-bit register H is 0 (2 machine cycles).
    fn cb_bit_4_h(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_h, 0x10);
    }

    /// Opcode 0x65: [BIT 4,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 4 of 8-bit register L is 0 (2 machine cycles).
    fn cb_bit_4_l(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_l, 0x10);
    }

    /// Opcode 0x66: [BIT 4,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=98)
    ///
    /// Test if the bit 4 of the data at the absolute address specified by the 16-bit
    /// register HL is 0 (3 machine cycles).
    fn cb_bit_4_hl(&mut self) {
        let operand = self.read_hl();
        self.run_bit_u8_and_update_flags(operand, 0x10);
    }

    /// Opcode 0x67: [BIT 4,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 4 of 8-bit register 1 is 0 (2 machine cycles).
    fn cb_bit_4_a(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_a, 0x10);
    }

    /// Opcode 0x68: [BIT 5,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 5 of 8-bit register B is 0 (2 machine cycles).
    fn cb_bit_5_b(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_b, 0x20);
    }

    /// Opcode 0x69: [BIT 5,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 5 of 8-bit register C is 0 (2 machine cycles).
    fn cb_bit_5_c(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_c, 0x20);
    }

    /// Opcode 0x6A: [BIT 5,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 5 of 8-bit register D is 0 (2 machine cycles).
    fn cb_bit_5_d(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_d, 0x20);
    }

    /// Opcode 0x6B: [BIT 5,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 5 of 8-bit register E is 0 (2 machine cycles).
    fn cb_bit_5_e(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_e, 0x20);
    }

    /// Opcode 0x6C: [BIT 5,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 5 of 8-bit register H is 0 (2 machine cycles).
    fn cb_bit_5_h(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_h, 0x20);
    }

    /// Opcode 0x6D: [BIT 5,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 5 of 8-bit register L is 0 (2 machine cycles).
    fn cb_bit_5_l(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_l, 0x20);
    }

    /// Opcode 0x6E: [BIT 5,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=98)
    ///
    /// Test if the bit 5 of the data at the absolute address specified by the 16-bit
    /// register HL is 0 (3 machine cycles).
    fn cb_bit_5_hl(&mut self) {
        let operand = self.read_hl();
        self.run_bit_u8_and_update_flags(operand, 0x20);
    }

    /// Opcode 0x6F: [BIT 5,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 5 of 8-bit register 1 is 0 (2 machine cycles).
    fn cb_bit_5_a(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_a, 0x20);
    }

    /// Opcode 0x70: [BIT 6,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 6 of 8-bit register B is 0 (2 machine cycles).
    fn cb_bit_6_b(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_b, 0x40);
    }

    /// Opcode 0x71: [BIT 6,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 6 of 8-bit register C is 0 (2 machine cycles).
    fn cb_bit_6_c(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_c, 0x40);
    }

    /// Opcode 0x72: [BIT 6,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 6 of 8-bit register D is 0 (2 machine cycles).
    fn cb_bit_6_d(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_d, 0x40);
    }

    /// Opcode 0x73: [BIT 6,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 6 of 8-bit register E is 0 (2 machine cycles).
    fn cb_bit_6_e(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_e, 0x40);
    }

    /// Opcode 0x74: [BIT 6,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 6 of 8-bit register H is 0 (2 machine cycles).
    fn cb_bit_6_h(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_h, 0x40);
    }

    /// Opcode 0x75: [BIT 6,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 6 of 8-bit register L is 0 (2 machine cycles).
    fn cb_bit_6_l(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_l, 0x40);
    }

    /// Opcode 0x76: [BIT 6,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=98)
    ///
    /// Test if the bit 6 of the data at the absolute address specified by the 16-bit
    /// register HL is 0 (3 machine cycles).
    fn cb_bit_6_hl(&mut self) {
        let operand = self.read_hl();
        self.run_bit_u8_and_update_flags(operand, 0x40);
    }

    /// Opcode 0x77: [BIT 6,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 6 of 8-bit register 1 is 0 (2 machine cycles).
    fn cb_bit_6_a(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_a, 0x40);
    }

    /// Opcode 0x78: [BIT 7,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 7 of 8-bit register B is 0 (2 machine cycles).
    fn cb_bit_7_b(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_b, 0x80);
    }

    /// Opcode 0x79: [BIT 7,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 7 of 8-bit register C is 0 (2 machine cycles).
    fn cb_bit_7_c(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_c, 0x80);
    }

    /// Opcode 0x7A: [BIT 7,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 7 of 8-bit register D is 0 (2 machine cycles).
    fn cb_bit_7_d(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_d, 0x80);
    }

    /// Opcode 0x7B: [BIT 7,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 7 of 8-bit register E is 0 (2 machine cycles).
    fn cb_bit_7_e(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_e, 0x80);
    }

    /// Opcode 0x7C: [BIT 7,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 7 of 8-bit register H is 0 (2 machine cycles).
    fn cb_bit_7_h(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_h, 0x80);
    }

    /// Opcode 0x7D: [BIT 7,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 7 of 8-bit register L is 0 (2 machine cycles).
    fn cb_bit_7_l(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_l, 0x80);
    }

    /// Opcode 0x7E: [BIT 7,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=98)
    ///
    /// Test if the bit 7 of the data at the absolute address specified by the 16-bit
    /// register HL is 0 (3 machine cycles).
    fn cb_bit_7_hl(&mut self) {
        let operand = self.read_hl();
        self.run_bit_u8_and_update_flags(operand, 0x80);
    }

    /// Opcode 0x7F: [BIT 7,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 7 of 8-bit register 1 is 0 (2 machine cycles).
    fn cb_bit_7_a(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.register_a, 0x80);
    }

    /// Opcode 0x80: [RES 0,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 0 of 8-bit register B (2 machine cycles).
    fn cb_res_0_b(&mut self) {
        self.registers.register_b &= 0xFE;
    }

    /// Opcode 0x81: [RES 0,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 0 of 8-bit register C (2 machine cycles).
    fn cb_res_0_c(&mut self) {
        self.registers.register_c &= 0xFE;
    }

    /// Opcode 0x82: [RES 0,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 0 of 8-bit register D (2 machine cycles).
    fn cb_res_0_d(&mut self) {
        self.registers.register_d &= 0xFE;
    }

    /// Opcode 0x83: [RES 0,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 0 of 8-bit register E (2 machine cycles).
    fn cb_res_0_e(&mut self) {
        self.registers.register_e &= 0xFE;
    }

    /// Opcode 0x84: [RES 0,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 0 of 8-bit register H (2 machine cycles).
    fn cb_res_0_h(&mut self) {
        self.registers.register_h &= 0xFE;
    }

    /// Opcode 0x85: [RES 0,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 0 of 8-bit register L (2 machine cycles).
    fn cb_res_0_l(&mut self) {
        self.registers.register_l &= 0xFE;
    }

    /// Opcode 0x86: [RES 0,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=100)
    ///
    /// Reset bit 0 of the data at the absolute address specified by the 16-bit register HL
    /// (3 machine cycles).
    fn cb_res_0_hl(&mut self) {
        let operand = self.read_hl();
        self.write_hl(operand & 0xFE);
    }

    /// Opcode 0x87: [RES 0,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 0 of 8-bit register A (2 machine cycles).
    fn cb_res_0_a(&mut self) {
        self.registers.register_a &= 0xFE;
    }

    /// Opcode 0x88: [RES 1,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 1 of 8-bit register B (2 machine cycles).
    fn cb_res_1_b(&mut self) {
        self.registers.register_b &= 0xFD;
    }

    /// Opcode 0x89: [RES 1,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 1 of 8-bit register C (2 machine cycles).
    fn cb_res_1_c(&mut self) {
        self.registers.register_c &= 0xFD;
    }

    /// Opcode 0x8A: [RES 1,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 1 of 8-bit register D (2 machine cycles).
    fn cb_res_1_d(&mut self) {
        self.registers.register_d &= 0xFD;
    }

    /// Opcode 0x8B: [RES 1,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 1 of 8-bit register E (2 machine cycles).
    fn cb_res_1_e(&mut self) {
        self.registers.register_e &= 0xFD;
    }

    /// Opcode 0x8C: [RES 1,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 1 of 8-bit register H (2 machine cycles).
    fn cb_res_1_h(&mut self) {
        self.registers.register_h &= 0xFD;
    }

    /// Opcode 0x8D: [RES 1,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 1 of 8-bit register L (2 machine cycles).
    fn cb_res_1_l(&mut self) {
        self.registers.register_l &= 0xFD;
    }

    /// Opcode 0x8E: [RES 1,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=100)
    ///
    /// Reset bit 1 of the data at the absolute address specified by the 16-bit register HL
    /// (3 machine cycles).
    fn cb_res_1_hl(&mut self) {
        let operand = self.read_hl();
        self.write_hl(operand & 0xFD);
    }

    /// Opcode 0x8F: [RES 1,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 1 of 8-bit register A (2 machine cycles).
    fn cb_res_1_a(&mut self) {
        self.registers.register_a &= 0xFD;
    }

    /// Opcode 0x90: [RES 2,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 2 of 8-bit register B (2 machine cycles).
    fn cb_res_2_b(&mut self) {
        self.registers.register_b &= 0xFB;
    }

    /// Opcode 0x91: [RES 2,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 2 of 8-bit register C (2 machine cycles).
    fn cb_res_2_c(&mut self) {
        self.registers.register_c &= 0xFB;
    }

    /// Opcode 0x92: [RES 2,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 2 of 8-bit register D (2 machine cycles).
    fn cb_res_2_d(&mut self) {
        self.registers.register_d &= 0xFB;
    }

    /// Opcode 0x93: [RES 2,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 2 of 8-bit register E (2 machine cycles).
    fn cb_res_2_e(&mut self) {
        self.registers.register_e &= 0xFB;
    }

    /// Opcode 0x94: [RES 2,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 2 of 8-bit register H (2 machine cycles).
    fn cb_res_2_h(&mut self) {
        self.registers.register_h &= 0xFB;
    }

    /// Opcode 0x95: [RES 2,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 2 of 8-bit register L (2 machine cycles).
    fn cb_res_2_l(&mut self) {
        self.registers.register_l &= 0xFB;
    }

    /// Opcode 0x96: [RES 2,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=100)
    ///
    /// Reset bit 2 of the data at the absolute address specified by the 16-bit register HL
    /// (3 machine cycles).
    fn cb_res_2_hl(&mut self) {
        let operand = self.read_hl();
        self.write_hl(operand & 0xFB);
    }

    /// Opcode 0x97: [RES 2,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 2 of 8-bit register A (2 machine cycles).
    fn cb_res_2_a(&mut self) {
        self.registers.register_a &= 0xFB;
    }

    /// Opcode 0x98: [RES 3,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 3 of 8-bit register B (2 machine cycles).
    fn cb_res_3_b(&mut self) {
        self.registers.register_b &= 0xF7;
    }

    /// Opcode 0x99: [RES 3,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 3 of 8-bit register C (2 machine cycles).
    fn cb_res_3_c(&mut self) {
        self.registers.register_c &= 0xF7;
    }

    /// Opcode 0x9A: [RES 3,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 3 of 8-bit register D (2 machine cycles).
    fn cb_res_3_d(&mut self) {
        self.registers.register_d &= 0xF7;
    }

    /// Opcode 0x9B: [RES 3,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 3 of 8-bit register E (2 machine cycles).
    fn cb_res_3_e(&mut self) {
        self.registers.register_e &= 0xF7;
    }

    /// Opcode 0x9C: [RES 3,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 3 of 8-bit register H (2 machine cycles).
    fn cb_res_3_h(&mut self) {
        self.registers.register_h &= 0xF7;
    }

    /// Opcode 0x9D: [RES 3,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 3 of 8-bit register L (2 machine cycles).
    fn cb_res_3_l(&mut self) {
        self.registers.register_l &= 0xF7;
    }

    /// Opcode 0x9E: [RES 3,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=100)
    ///
    /// Reset bit 3 of the data at the absolute address specified by the 16-bit register HL
    /// (3 machine cycles).
    fn cb_res_3_hl(&mut self) {
        let operand = self.read_hl();
        self.write_hl(operand & 0xF7);
    }

    /// Opcode 0x9F: [RES 3,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 3 of 8-bit register A (2 machine cycles).
    fn cb_res_3_a(&mut self) {
        self.registers.register_a &= 0xF7;
    }

    /// Opcode 0xA0: [RES 4,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 4 of 8-bit register B (2 machine cycles).
    fn cb_res_4_b(&mut self) {
        self.registers.register_b &= 0xEF;
    }

    /// Opcode 0xA1: [RES 4,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 4 of 8-bit register C (2 machine cycles).
    fn cb_res_4_c(&mut self) {
        self.registers.register_c &= 0xEF;
    }

    /// Opcode 0xA2: [RES 4,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 4 of 8-bit register D (2 machine cycles).
    fn cb_res_4_d(&mut self) {
        self.registers.register_d &= 0xEF;
    }

    /// Opcode 0xA3: [RES 4,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 4 of 8-bit register E (2 machine cycles).
    fn cb_res_4_e(&mut self) {
        self.registers.register_e &= 0xEF;
    }

    /// Opcode 0xA4: [RES 4,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 4 of 8-bit register H (2 machine cycles).
    fn cb_res_4_h(&mut self) {
        self.registers.register_h &= 0xEF;
    }

    /// Opcode 0xA5: [RES 4,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 4 of 8-bit register L (2 machine cycles).
    fn cb_res_4_l(&mut self) {
        self.registers.register_l &= 0xEF;
    }

    /// Opcode 0xA6: [RES 4,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=100)
    ///
    /// Reset bit 4 of the data at the absolute address specified by the 16-bit register HL
    /// (3 machine cycles).
    fn cb_res_4_hl(&mut self) {
        let operand = self.read_hl();
        self.write_hl(operand & 0xEF);
    }

    /// Opcode 0xA7: [RES 4,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 4 of 8-bit register A (2 machine cycles).
    fn cb_res_4_a(&mut self) {
        self.registers.register_a &= 0xEF;
    }

    /// Opcode 0xA8: [RES 5,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 5 of 8-bit register B (2 machine cycles).
    fn cb_res_5_b(&mut self) {
        self.registers.register_b &= 0xDF;
    }

    /// Opcode 0xA9: [RES 5,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 5 of 8-bit register C (2 machine cycles).
    fn cb_res_5_c(&mut self) {
        self.registers.register_c &= 0xDF;
    }

    /// Opcode 0xAA: [RES 5,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 5 of 8-bit register D (2 machine cycles).
    fn cb_res_5_d(&mut self) {
        self.registers.register_d &= 0xDF;
    }

    /// Opcode 0xAB: [RES 5,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 5 of 8-bit register E (2 machine cycles).
    fn cb_res_5_e(&mut self) {
        self.registers.register_e &= 0xDF;
    }

    /// Opcode 0xAC: [RES 5,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 5 of 8-bit register H (2 machine cycles).
    fn cb_res_5_h(&mut self) {
        self.registers.register_h &= 0xDF;
    }

    /// Opcode 0xAD: [RES 5,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 5 of 8-bit register L (2 machine cycles).
    fn cb_res_5_l(&mut self) {
        self.registers.register_l &= 0xDF;
    }

    /// Opcode 0xAE: [RES 5,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=100)
    ///
    /// Reset bit 5 of the data at the absolute address specified by the 16-bit register HL
    /// (3 machine cycles).
    fn cb_res_5_hl(&mut self) {
        let operand = self.read_hl();
        self.write_hl(operand & 0xDF);
    }

    /// Opcode 0xAF: [RES 5,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 5 of 8-bit register A (2 machine cycles).
    fn cb_res_5_a(&mut self) {
        self.registers.register_a &= 0xDF;
    }

    /// Opcode 0xB0: [RES 6,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 6 of 8-bit register B (2 machine cycles).
    fn cb_res_6_b(&mut self) {
        self.registers.register_b &= 0xBF;
    }

    /// Opcode 0xB1: [RES 6,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 6 of 8-bit register C (2 machine cycles).
    fn cb_res_6_c(&mut self) {
        self.registers.register_c &= 0xBF;
    }

    /// Opcode 0xB2: [RES 6,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 6 of 8-bit register D (2 machine cycles).
    fn cb_res_6_d(&mut self) {
        self.registers.register_d &= 0xBF;
    }

    /// Opcode 0xB3: [RES 6,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 6 of 8-bit register E (2 machine cycles).
    fn cb_res_6_e(&mut self) {
        self.registers.register_e &= 0xBF;
    }

    /// Opcode 0xB4: [RES 6,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 6 of 8-bit register H (2 machine cycles).
    fn cb_res_6_h(&mut self) {
        self.registers.register_h &= 0xBF;
    }

    /// Opcode 0xB5: [RES 6,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 6 of 8-bit register L (2 machine cycles).
    fn cb_res_6_l(&mut self) {
        self.registers.register_l &= 0xBF;
    }

    /// Opcode 0xB6: [RES 6,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=100)
    ///
    /// Reset bit 6 of the data at the absolute address specified by the 16-bit register HL
    /// (3 machine cycles).
    fn cb_res_6_hl(&mut self) {
        let operand = self.read_hl();
        self.write_hl(operand & 0xBF);
    }

    /// Opcode 0xB7: [RES 6,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 6 of 8-bit register A (2 machine cycles).
    fn cb_res_6_a(&mut self) {
        self.registers.register_a &= 0xBF;
    }

    /// Opcode 0xB8: [RES 7,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 7 of 8-bit register B (2 machine cycles).
    fn cb_res_7_b(&mut self) {
        self.registers.register_b &= 0x7F;
    }

    /// Opcode 0xB9: [RES 7,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 7 of 8-bit register C (2 machine cycles).
    fn cb_res_7_c(&mut self) {
        self.registers.register_c &= 0x7F;
    }

    /// Opcode 0xBA: [RES 7,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 7 of 8-bit register D (2 machine cycles).
    fn cb_res_7_d(&mut self) {
        self.registers.register_d &= 0x7F;
    }

    /// Opcode 0xBB: [RES 7,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 7 of 8-bit register E (2 machine cycles).
    fn cb_res_7_e(&mut self) {
        self.registers.register_e &= 0x7F;
    }

    /// Opcode 0xBC: [RES 7,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 7 of 8-bit register H (2 machine cycles).
    fn cb_res_7_h(&mut self) {
        self.registers.register_h &= 0x7F;
    }

    /// Opcode 0xBD: [RES 7,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 7 of 8-bit register L (2 machine cycles).
    fn cb_res_7_l(&mut self) {
        self.registers.register_l &= 0x7F;
    }

    /// Opcode 0xBE: [RES 7,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=100)
    ///
    /// Reset bit 7 of the data at the absolute address specified by the 16-bit register HL
    /// (3 machine cycles).
    fn cb_res_7_hl(&mut self) {
        let operand = self.read_hl();
        self.write_hl(operand & 0x7F);
    }

    /// Opcode 0xBF: [RES 7,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 7 of 8-bit register A (2 machine cycles).
    fn cb_res_7_a(&mut self) {
        self.registers.register_a &= 0x7F;
    }

    /// Opcode 0xC0: [SET 0,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 0 of 8-bit register B (2 machine cycles).
    fn cb_set_0_b(&mut self) {
        self.registers.register_b |= 0x01;
    }

    /// Opcode 0xC1: [SET 0,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 0 of 8-bit register C (2 machine cycles).
    fn cb_set_0_c(&mut self) {
        self.registers.register_c |= 0x01;
    }

    /// Opcode 0xC2: [SET 0,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 0 of 8-bit register D (2 machine cycles).
    fn cb_set_0_d(&mut self) {
        self.registers.register_d |= 0x01;
    }

    /// Opcode 0xC3: [SET 0,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 0 of 8-bit register E (2 machine cycles).
    fn cb_set_0_e(&mut self) {
        self.registers.register_e |= 0x01;
    }

    /// Opcode 0xC4: [SET 0,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 0 of 8-bit register H (2 machine cycles).
    fn cb_set_0_h(&mut self) {
        self.registers.register_h |= 0x01;
    }

    /// Opcode 0xC5: [SET 0,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 0 of 8-bit register L (2 machine cycles).
    fn cb_set_0_l(&mut self) {
        self.registers.register_l |= 0x01;
    }

    /// Opcode 0xC6: [SET 0,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=102)
    ///
    /// Set bit 0 of the data at the absolute address specified by the 16-bit register HL
    /// (3 machine cycles).
    fn cb_set_0_hl(&mut self) {
        let operand = self.read_hl();
        self.write_hl(operand | 0x01);
    }

    /// Opcode 0xC7: [SET 0,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 0 of 8-bit register A (2 machine cycles).
    fn cb_set_0_a(&mut self) {
        self.registers.register_a |= 0x01;
    }

    /// Opcode 0xC8: [SET 1,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 1 of 8-bit register B (2 machine cycles).
    fn cb_set_1_b(&mut self) {
        self.registers.register_b |= 0x02;
    }

    /// Opcode 0xC9: [SET 1,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 1 of 8-bit register C (2 machine cycles).
    fn cb_set_1_c(&mut self) {
        self.registers.register_c |= 0x02;
    }

    /// Opcode 0xCA: [SET 1,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 1 of 8-bit register D (2 machine cycles).
    fn cb_set_1_d(&mut self) {
        self.registers.register_d |= 0x02;
    }

    /// Opcode 0xCB: [SET 1,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 1 of 8-bit register E (2 machine cycles).
    fn cb_set_1_e(&mut self) {
        self.registers.register_e |= 0x02;
    }

    /// Opcode 0xCC: [SET 1,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 1 of 8-bit register H (2 machine cycles).
    fn cb_set_1_h(&mut self) {
        self.registers.register_h |= 0x02;
    }

    /// Opcode 0xCD: [SET 1,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 1 of 8-bit register L (2 machine cycles).
    fn cb_set_1_l(&mut self) {
        self.registers.register_l |= 0x02;
    }

    /// Opcode 0xCE: [SET 1,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=102)
    ///
    /// Set bit 1 of the data at the absolute address specified by the 16-bit register HL
    /// (3 machine cycles).
    fn cb_set_1_hl(&mut self) {
        let operand = self.read_hl();
        self.write_hl(operand | 0x02);
    }

    /// Opcode 0xCF: [SET 1,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 1 of 8-bit register A (2 machine cycles).
    fn cb_set_1_a(&mut self) {
        self.registers.register_a |= 0x02;
    }

    /// Opcode 0xD0: [SET 2,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 2 of 8-bit register B (2 machine cycles).
    fn cb_set_2_b(&mut self) {
        self.registers.register_b |= 0x04;
    }

    /// Opcode 0xD1: [SET 2,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 2 of 8-bit register C (2 machine cycles).
    fn cb_set_2_c(&mut self) {
        self.registers.register_c |= 0x04;
    }

    /// Opcode 0xD2: [SET 2,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 2 of 8-bit register D (2 machine cycles).
    fn cb_set_2_d(&mut self) {
        self.registers.register_d |= 0x04;
    }

    /// Opcode 0xD3: [SET 2,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 2 of 8-bit register E (2 machine cycles).
    fn cb_set_2_e(&mut self) {
        self.registers.register_e |= 0x04;
    }

    /// Opcode 0xD4: [SET 2,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 2 of 8-bit register H (2 machine cycles).
    fn cb_set_2_h(&mut self) {
        self.registers.register_h |= 0x04;
    }

    /// Opcode 0xD5: [SET 2,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 2 of 8-bit register L (2 machine cycles).
    fn cb_set_2_l(&mut self) {
        self.registers.register_l |= 0x04;
    }

    /// Opcode 0xD6: [SET 2,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=102)
    ///
    /// Set bit 2 of the data at the absolute address specified by the 16-bit register HL
    /// (3 machine cycles).
    fn cb_set_2_hl(&mut self) {
        let operand = self.read_hl();
        self.write_hl(operand | 0x04);
    }

    /// Opcode 0xD7: [SET 2,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 2 of 8-bit register A (2 machine cycles).
    fn cb_set_2_a(&mut self) {
        self.registers.register_a |= 0x04;
    }

    /// Opcode 0xD8: [SET 3,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 3 of 8-bit register B (2 machine cycles).
    fn cb_set_3_b(&mut self) {
        self.registers.register_b |= 0x08;
    }

    /// Opcode 0xD9: [SET 3,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 3 of 8-bit register C (2 machine cycles).
    fn cb_set_3_c(&mut self) {
        self.registers.register_c |= 0x08;
    }

    /// Opcode 0xDA: [SET 3,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 3 of 8-bit register D (2 machine cycles).
    fn cb_set_3_d(&mut self) {
        self.registers.register_d |= 0x08;
    }

    /// Opcode 0xDB: [SET 3,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 3 of 8-bit register E (2 machine cycles).
    fn cb_set_3_e(&mut self) {
        self.registers.register_e |= 0x08;
    }

    /// Opcode 0xDC: [SET 3,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 3 of 8-bit register H (2 machine cycles).
    fn cb_set_3_h(&mut self) {
        self.registers.register_h |= 0x08;
    }

    /// Opcode 0xDD: [SET 3,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 3 of 8-bit register L (2 machine cycles).
    fn cb_set_3_l(&mut self) {
        self.registers.register_l |= 0x08;
    }

    /// Opcode 0xDE: [SET 3,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=102)
    ///
    /// Set bit 3 of the data at the absolute address specified by the 16-bit register HL
    /// (3 machine cycles).
    fn cb_set_3_hl(&mut self) {
        let operand = self.read_hl();
        self.write_hl(operand | 0x08);
    }

    /// Opcode 0xDF: [SET 3,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 3 of 8-bit register A (2 machine cycles).
    fn cb_set_3_a(&mut self) {
        self.registers.register_a |= 0x08;
    }

    /// Opcode 0xE0: [SET 4,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 4 of 8-bit register B (2 machine cycles).
    fn cb_set_4_b(&mut self) {
        self.registers.register_b |= 0x10;
    }

    /// Opcode 0xE1: [SET 4,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 4 of 8-bit register C (2 machine cycles).
    fn cb_set_4_c(&mut self) {
        self.registers.register_c |= 0x10;
    }

    /// Opcode 0xE2: [SET 4,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 4 of 8-bit register D (2 machine cycles).
    fn cb_set_4_d(&mut self) {
        self.registers.register_d |= 0x10;
    }

    /// Opcode 0xE3: [SET 4,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 4 of 8-bit register E (2 machine cycles).
    fn cb_set_4_e(&mut self) {
        self.registers.register_e |= 0x10;
    }

    /// Opcode 0xE4: [SET 4,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 4 of 8-bit register H (2 machine cycles).
    fn cb_set_4_h(&mut self) {
        self.registers.register_h |= 0x10;
    }

    /// Opcode 0xE5: [SET 4,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 4 of 8-bit register L (2 machine cycles).
    fn cb_set_4_l(&mut self) {
        self.registers.register_l |= 0x10;
    }

    /// Opcode 0xE6: [SET 4,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=102)
    ///
    /// Set bit 4 of the data at the absolute address specified by the 16-bit register HL
    /// (3 machine cycles).
    fn cb_set_4_hl(&mut self) {
        let operand = self.read_hl();
        self.write_hl(operand | 0x10);
    }

    /// Opcode 0xE7: [SET 4,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 4 of 8-bit register A (2 machine cycles).
    fn cb_set_4_a(&mut self) {
        self.registers.register_a |= 0x10;
    }

    /// Opcode 0xE8: [SET 5,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 5 of 8-bit register B (2 machine cycles).
    fn cb_set_5_b(&mut self) {
        self.registers.register_b |= 0x20;
    }

    /// Opcode 0xE9: [SET 5,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 5 of 8-bit register C (2 machine cycles).
    fn cb_set_5_c(&mut self) {
        self.registers.register_c |= 0x20;
    }

    /// Opcode 0xEA: [SET 5,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 5 of 8-bit register D (2 machine cycles).
    fn cb_set_5_d(&mut self) {
        self.registers.register_d |= 0x20;
    }

    /// Opcode 0xEB: [SET 5,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 5 of 8-bit register E (2 machine cycles).
    fn cb_set_5_e(&mut self) {
        self.registers.register_e |= 0x20;
    }

    /// Opcode 0xEC: [SET 5,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 5 of 8-bit register H (2 machine cycles).
    fn cb_set_5_h(&mut self) {
        self.registers.register_h |= 0x20;
    }

    /// Opcode 0xED: [SET 5,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 5 of 8-bit register L (2 machine cycles).
    fn cb_set_5_l(&mut self) {
        self.registers.register_l |= 0x20;
    }

    /// Opcode 0xEE: [SET 5,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=102)
    ///
    /// Set bit 5 of the data at the absolute address specified by the 16-bit register HL
    /// (3 machine cycles).
    fn cb_set_5_hl(&mut self) {
        let operand = self.read_hl();
        self.write_hl(operand | 0x20);
    }

    /// Opcode 0xEF: [SET 5,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 5 of 8-bit register A (2 machine cycles).
    fn cb_set_5_a(&mut self) {
        self.registers.register_a |= 0x20;
    }

    /// Opcode 0xF0: [SET 6,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 6 of 8-bit register B (2 machine cycles).
    fn cb_set_6_b(&mut self) {
        self.registers.register_b |= 0x40;
    }

    /// Opcode 0xF1: [SET 6,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 6 of 8-bit register C (2 machine cycles).
    fn cb_set_6_c(&mut self) {
        self.registers.register_c |= 0x40;
    }

    /// Opcode 0xF2: [SET 6,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 6 of 8-bit register D (2 machine cycles).
    fn cb_set_6_d(&mut self) {
        self.registers.register_d |= 0x40;
    }

    /// Opcode 0xF3: [SET 6,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 6 of 8-bit register E (2 machine cycles).
    fn cb_set_6_e(&mut self) {
        self.registers.register_e |= 0x40;
    }

    /// Opcode 0xF4: [SET 6,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 6 of 8-bit register H (2 machine cycles).
    fn cb_set_6_h(&mut self) {
        self.registers.register_h |= 0x40;
    }

    /// Opcode 0xF5: [SET 6,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 6 of 8-bit register L (2 machine cycles).
    fn cb_set_6_l(&mut self) {
        self.registers.register_l |= 0x40;
    }

    /// Opcode 0xF6: [SET 6,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=102)
    ///
    /// Set bit 6 of the data at the absolute address specified by the 16-bit register HL
    /// (3 machine cycles).
    fn cb_set_6_hl(&mut self) {
        let operand = self.read_hl();
        self.write_hl(operand | 0x40);
    }

    /// Opcode 0xF7: [SET 6,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 6 of 8-bit register A (2 machine cycles).
    fn cb_set_6_a(&mut self) {
        self.registers.register_a |= 0x40;
    }

    /// Opcode 0xF8: [SET 7,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 7 of 8-bit register B (2 machine cycles).
    fn cb_set_7_b(&mut self) {
        self.registers.register_b |= 0x80;
    }

    /// Opcode 0xF9: [SET 7,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 7 of 8-bit register C (2 machine cycles).
    fn cb_set_7_c(&mut self) {
        self.registers.register_c |= 0x80;
    }

    /// Opcode 0xFA: [SET 7,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 7 of 8-bit register D (2 machine cycles).
    fn cb_set_7_d(&mut self) {
        self.registers.register_d |= 0x80;
    }

    /// Opcode 0xFB: [SET 7,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 7 of 8-bit register E (2 machine cycles).
    fn cb_set_7_e(&mut self) {
        self.registers.register_e |= 0x80;
    }

    /// Opcode 0xFC: [SET 7,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 7 of 8-bit register H (2 machine cycles).
    fn cb_set_7_h(&mut self) {
        self.registers.register_h |= 0x80;
    }

    /// Opcode 0xFD: [SET 7,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 7 of 8-bit register L (2 machine cycles).
    fn cb_set_7_l(&mut self) {
        self.registers.register_l |= 0x80;
    }

    /// Opcode 0xFE: [SET 7,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=102)
    ///
    /// Set bit 7 of the data at the absolute address specified by the 16-bit register HL
    /// (3 machine cycles).
    fn cb_set_7_hl(&mut self) {
        let operand = self.read_hl();
        self.write_hl(operand | 0x80);
    }

    /// Opcode 0xFF: [SET 7,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 7 of 8-bit register A (2 machine cycles).
    fn cb_set_7_a(&mut self) {
        self.registers.register_a |= 0x80;
    }
}
