use super::memory::Memory;

const OP_CODE_FUNCTION_TABLE: [fn(&mut Cpu); 256] = [
    Cpu::op_nop,         // 0x00 : NOP
    Cpu::op_ld_bc_u16,   // 0x01 : LD BC,d16
    Cpu::op_ld_bc_a,     // 0x02 : LD (BC),A
    Cpu::op_inc_bc,      // 0x03 : INC BC
    Cpu::op_inc_b,       // 0x04 : INC B
    Cpu::op_dec_b,       // 0x05 : DEC B
    Cpu::op_ld_b_d8,     // 0x06 : LD B,d8
    Cpu::op_placeholder, // 0x07 : RLCA
    Cpu::op_ld_u16_sp,   // 0x08 : LD (a16),SP
    Cpu::op_add_hl_bc,   // 0x09 : ADD HL,BC
    Cpu::op_ld_a_bc,     // 0x0A : LD A,(BC)
    Cpu::op_dec_bc,      // 0x0B : DEC BC
    Cpu::op_inc_c,       // 0x0C : INC C
    Cpu::op_dec_c,       // 0x0D : DEC C
    Cpu::op_ld_c_d8,     // 0x0E : LD C,d8
    Cpu::op_placeholder, // 0x0F : RRCA
    Cpu::op_placeholder, // 0x10 : STOP 0
    Cpu::op_ld_de_u16,   // 0x11 : LD DE,d16
    Cpu::op_ld_de_a,     // 0x12 : LD (DE),A
    Cpu::op_inc_de,      // 0x13 : INC DE
    Cpu::op_inc_d,       // 0x14 : INC D
    Cpu::op_dec_d,       // 0x15 : DEC D
    Cpu::op_ld_d_d8,     // 0x16 : LD D,d8
    Cpu::op_placeholder, // 0x17 : RLA
    Cpu::op_placeholder, // 0x18 : JR r8
    Cpu::op_add_hl_de,   // 0x19 : ADD HL,DE
    Cpu::op_ld_a_de,     // 0x1A : LD A,(DE)
    Cpu::op_dec_de,      // 0x1B : DEC DE
    Cpu::op_inc_e,       // 0x1C : INC E
    Cpu::op_dec_e,       // 0x1D : DEC E
    Cpu::op_ld_e_d8,     // 0x1E : LD E,d8
    Cpu::op_placeholder, // 0x1F : RRA
    Cpu::op_placeholder, // 0x20 : JR NZ,r8
    Cpu::op_ld_hl_u16,   // 0x21 : LD HL,d16
    Cpu::op_ld_hl_inc_a, // 0x22 : LD (HL+),A
    Cpu::op_inc_hl,      // 0x23 : INC HL
    Cpu::op_inc_h,       // 0x24 : INC H
    Cpu::op_dec_h,       // 0x25 : DEC H
    Cpu::op_ld_h_d8,     // 0x26 : LD H,d8
    Cpu::op_daa,         // 0x27 : DAA
    Cpu::op_placeholder, // 0x28 : JR Z,r8
    Cpu::op_add_hl_hl,   // 0x29 : ADD HL,HL
    Cpu::op_ld_a_hl_inc, // 0x2A : LD A,(HL+)
    Cpu::op_dec_hl,      // 0x2B : DEC HL
    Cpu::op_inc_l,       // 0x2C : INC L
    Cpu::op_dec_l,       // 0x2D : DEC L
    Cpu::op_ld_l_d8,     // 0x2E : LD L,d8
    Cpu::op_cpl,         // 0x2F : CPL
    Cpu::op_placeholder, // 0x30 : JR NC,r8
    Cpu::op_ld_sp_u16,   // 0x31 : LD SP,d16
    Cpu::op_ld_hl_dec_a, // 0x32 : LD (HL-),A
    Cpu::op_inc_sp,      // 0x33 : INC SP
    Cpu::op_inc_hl_ind,  // 0x34 : INC (HL)
    Cpu::op_dec_hl_ind,  // 0x35 : DEC (HL)
    Cpu::op_ld_hl_d8,    // 0x36 : LD (HL),d8
    Cpu::op_scf,         // 0x37 : SCF
    Cpu::op_placeholder, // 0x38 : JR C,r8
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
    Cpu::op_placeholder, // 0xC0 : RET NZ
    Cpu::op_pop_bc,      // 0xC1 : POP BC
    Cpu::op_placeholder, // 0xC2 : JP NZ,a16
    Cpu::op_placeholder, // 0xC3 : JP a16
    Cpu::op_placeholder, // 0xC4 : CALL NZ,a16
    Cpu::op_push_bc,     // 0xC5 : PUSH BC
    Cpu::op_add_a_u8,    // 0xC6 : ADD A,d8
    Cpu::op_placeholder, // 0xC7 : RST 00H
    Cpu::op_placeholder, // 0xC8 : RET Z
    Cpu::op_placeholder, // 0xC9 : RET
    Cpu::op_placeholder, // 0xCA : JP Z,a16
    Cpu::op_placeholder, // 0xCB : PREFIX CB
    Cpu::op_placeholder, // 0xCC : CALL Z,a16
    Cpu::op_placeholder, // 0xCD : CALL a16
    Cpu::op_adc_a_u8,    // 0xCE : ADC A,d8
    Cpu::op_placeholder, // 0xCF : RST 08H
    Cpu::op_placeholder, // 0xD0 : RET NC
    Cpu::op_pop_de,      // 0xD1 : POP DE
    Cpu::op_placeholder, // 0xD2 : JP NC,a16
    Cpu::op_placeholder, // 0xD3 : undefined
    Cpu::op_placeholder, // 0xD4 : CALL NC,a16
    Cpu::op_push_de,     // 0xD5 : PUSH DE
    Cpu::op_sub_a_u8,    // 0xD6 : SUB d8
    Cpu::op_placeholder, // 0xD7 : RST 10H
    Cpu::op_placeholder, // 0xD8 : RET C
    Cpu::op_placeholder, // 0xD9 : RETI
    Cpu::op_placeholder, // 0xDA : JP C,a16
    Cpu::op_placeholder, // 0xDB : undefined
    Cpu::op_placeholder, // 0xDC : CALL C,a16
    Cpu::op_placeholder, // 0xDD : undefined
    Cpu::op_sbc_a_u8,    // 0xDE : SBC A,d8
    Cpu::op_placeholder, // 0xDF : RST 18H
    Cpu::op_ldh_a8_a,    // 0xE0 : LDH (a8),A
    Cpu::op_pop_hl,      // 0xE1 : POP HL
    Cpu::op_placeholder, // 0xE2 : LD (C),A
    Cpu::op_placeholder, // 0xE3 : undefined
    Cpu::op_placeholder, // 0xE4 : undefined
    Cpu::op_push_hl,     // 0xE5 : PUSH HL
    Cpu::op_and_a_u8,    // 0xE6 : AND d8
    Cpu::op_placeholder, // 0xE7 : RST 20H
    Cpu::op_add_sp_i8,   // 0xE8 : ADD SP,r8
    Cpu::op_placeholder, // 0xE9 : JP (HL)
    Cpu::op_ld_a16_a,    // 0xEA : LD (a16),A
    Cpu::op_placeholder, // 0xEB : undefined
    Cpu::op_placeholder, // 0xEC : undefined
    Cpu::op_placeholder, // 0xED : undefined
    Cpu::op_xor_a_u8,    // 0xEE : XOR d8
    Cpu::op_placeholder, // 0xEF : RST 28H
    Cpu::op_ldh_a_a8,    // 0xF0 : LDH A,(a8)
    Cpu::op_pop_af,      // 0xF1 : POP AF
    Cpu::op_placeholder, // 0xF2 : LD A,(C)
    Cpu::op_placeholder, // 0xF3 : DI
    Cpu::op_placeholder, // 0xF4 : undefined
    Cpu::op_push_af,     // 0xF5 : PUSH AF
    Cpu::op_or_a_u8,     // 0xF6 : OR d8
    Cpu::op_placeholder, // 0xF7 : RST 30H
    Cpu::op_ld_hl_sp_i8, // 0xF8 : LD HL,SP+r8
    Cpu::op_ld_sp_hl,    // 0xF9 : LD SP,HL
    Cpu::op_ld_a_a16,    // 0xFA : LD A,(a16)
    Cpu::op_placeholder, // 0xFB : EI
    Cpu::op_placeholder, // 0xFC : undefined
    Cpu::op_placeholder, // 0xFD : undefined
    Cpu::op_cp_a_u8,     // 0xFE : CP d8
    Cpu::op_placeholder, // 0xFF : RST 38H
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
}

impl Cpu {
    pub fn new(memory: Box<dyn Memory>) -> Self {
        Self {
            memory,
            program_counter: 0,
            registers: CpuRegisters::new(),
            status_flags: 0,
            stack_pointer: 0,
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

    fn stack_push_u16(&mut self, value: u16) {
        self.stack_push_u8((value >> 8) as u8);
        self.stack_push_u8((value & 0xFF) as u8);
    }

    fn run_add_and_update_flags(&mut self, operand: u8) {
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
    /// Increments data in the 8-bit register B (1 machine cycles).
    fn op_inc_b(&mut self) {
        self.registers.register_b = self.run_inc_u8_and_update_flags(self.registers.register_b);
    }

    /// Opcode 0x05: [DEC B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=57)
    ///
    /// Decrements data in the 8-bit register B (1 machine cycles).
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
    /// Increments data in the 8-bit register C (1 machine cycles).
    fn op_inc_c(&mut self) {
        self.registers.register_c = self.run_inc_u8_and_update_flags(self.registers.register_c);
    }

    /// Opcode 0x0D: [DEC C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=57)
    ///
    /// Decrements data in the 8-bit register C (1 machine cycles).
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
    /// Increments data in the 8-bit register D (1 machine cycles).
    fn op_inc_d(&mut self) {
        self.registers.register_d = self.run_inc_u8_and_update_flags(self.registers.register_d);
    }

    /// Opcode 0x15: [DEC D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=57)
    ///
    /// Decrements data in the 8-bit register D (1 machine cycles).
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
    /// Increments data in the 8-bit register E (1 machine cycles).
    fn op_inc_e(&mut self) {
        self.registers.register_e = self.run_inc_u8_and_update_flags(self.registers.register_e);
    }

    /// Opcode 0x1D: [DEC E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=57)
    ///
    /// Decrements data in the 8-bit register E (1 machine cycles).
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
    /// Increments data in the 8-bit register H (1 machine cycles).
    fn op_inc_h(&mut self) {
        self.registers.register_h = self.run_inc_u8_and_update_flags(self.registers.register_h);
    }

    /// Opcode 0x25: [DEC H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=57)
    ///
    /// Decrements data in the 8-bit register H (1 machine cycles).
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
    /// Increments data in the 8-bit register L (1 machine cycles).
    fn op_inc_l(&mut self) {
        self.registers.register_l = self.run_inc_u8_and_update_flags(self.registers.register_l);
    }

    /// Opcode 0x2D: [DEC L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=57)
    ///
    /// Decrements data in the 8-bit register L (1 machine cycles).
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
        let value = self.run_inc_u8_and_update_flags(self.memory.read(address));
        self.memory.write(address, value);
    }

    /// Opcode 0x35: [DEC (HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=58)
    ///
    /// Decrements data at the absolute address specified by the 16-bit register HL (3
    /// machine cycles).
    fn op_dec_hl_ind(&mut self) {
        let address = self.registers.hl();
        let value = self.run_dec_u8_and_update_flags(self.memory.read(address));
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
    /// Increments data in the 8-bit register A (1 machine cycles).
    fn op_inc_a(&mut self) {
        self.registers.register_a = self.run_inc_u8_and_update_flags(self.registers.register_a);
    }

    /// Opcode 0x3D: [DEC A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=57)
    ///
    /// Decrements data in the 8-bit register A (1 machine cycles).
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
        self.run_add_and_update_flags(self.registers.register_b);
    }

    /// Opcode 0x81: [ADD A,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=40)
    ///
    /// Adds to the 8-bit A register, the 8-bit register C, and stores the result back
    /// into the A register (1 machine cycle).
    fn op_add_a_c(&mut self) {
        self.run_add_and_update_flags(self.registers.register_c);
    }

    /// Opcode 0x82: [ADD A,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=40)
    ///
    /// Adds to the 8-bit A register, the 8-bit register D, and stores the result back
    /// into the A register (1 machine cycle).
    fn op_add_a_d(&mut self) {
        self.run_add_and_update_flags(self.registers.register_d);
    }

    /// Opcode 0x83: [ADD A,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=40)
    ///
    /// Adds to the 8-bit A register, the 8-bit register E, and stores the result back
    /// into the A register (1 machine cycle).
    fn op_add_a_e(&mut self) {
        self.run_add_and_update_flags(self.registers.register_e);
    }

    /// Opcode 0x84: [ADD A,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=40)
    ///
    /// Adds to the 8-bit A register, the 8-bit register H, and stores the result back
    /// into the A register (1 machine cycle).
    fn op_add_a_h(&mut self) {
        self.run_add_and_update_flags(self.registers.register_h);
    }

    /// Opcode 0x85: [ADD A,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=40)
    ///
    /// Adds to the 8-bit A register, the 8-bit register L, and stores the result back
    /// into the A register (1 machine cycle).
    fn op_add_a_l(&mut self) {
        self.run_add_and_update_flags(self.registers.register_l);
    }

    /// Opcode 0x86: [ADD A,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=41)
    ///
    /// Adds to the 8-bit A register, data from the absolute address specified by the
    /// 16-bit register HL, and stores the result back into the A register (2 machine
    /// cycles).
    fn op_add_a_hl(&mut self) {
        let operand = self.read_hl();
        self.run_add_and_update_flags(operand);
    }

    /// Opcode 0x87: [ADD A,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=40)
    ///
    /// Adds to the 8-bit A register, the 8-bit register A, and stores the result back
    /// into the A register (1 machine cycle).
    fn op_add_a_a(&mut self) {
        self.run_add_and_update_flags(self.registers.register_a);
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

    /// Opcode 0xC1: [POP BC](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=38)
    ///
    /// Pops to the 16-bit register BC, data from the stack memory (3 machine cycles).
    fn op_pop_bc(&mut self) {
        self.registers.register_c = self.stack_pop_u8();
        self.registers.register_b = self.stack_pop_u8();
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
        self.run_add_and_update_flags(operand);
    }

    /// Opcode 0xCE: [ADC A,d8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=45)
    ///
    /// Adds to the 8-bit A register, the carry flag and the immediate data following
    /// the opcode, and stores the result back into the A register (2 machine cycles).
    fn op_adc_a_u8(&mut self) {
        let operand = self.fetch_u8();
        self.run_adc_and_update_flags(operand);
    }

    /// Opcode 0xD1: [POP DE](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=38)
    ///
    /// Pops to the 16-bit register DE, data from the stack memory (3 machine cycles).
    fn op_pop_de(&mut self) {
        self.registers.register_e = self.stack_pop_u8();
        self.registers.register_d = self.stack_pop_u8();
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

    /// Opcode 0xDE: [SBC A,d8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=51)
    ///
    /// Subtracts from the 8-bit A register, the carry flag and the immediate data
    /// following the opcode, and stores the result back into the A register (2 machine
    /// cycles).
    fn op_sbc_a_u8(&mut self) {
        let operand = self.fetch_u8();
        self.run_sbc_and_update_flags(operand);
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

    /// Opcode 0xFE: [CP d8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=54)
    ///
    /// Subtracts from the 8-bit A register, the immediate data n, and updates flags
    /// based on the result. This instruction is basically identical to SUB d8, but
    /// does not update the A register (2 machine cycles).
    fn op_cp_a_u8(&mut self) {
        let operand = self.fetch_u8();
        self.run_cp_and_update_flags(operand);
    }
}

impl Cpu {
    // TODO: This op is supposed to be 2 machine cycles long, is there a dummy cycle?
    fn op_add_hl(&mut self, operand: u16) {
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

    fn op_add_hl_bc(&mut self) {
        self.op_add_hl(self.registers.bc());
    }

    fn op_add_hl_de(&mut self) {
        self.op_add_hl(self.registers.de());
    }

    fn op_add_hl_hl(&mut self) {
        self.op_add_hl(self.registers.hl());
    }

    fn op_add_hl_sp(&mut self) {
        self.op_add_hl(self.stack_pointer);
    }

    // TODO: This op is supposed to be 4 machine cycles long, needs 2 extra dummy cycles
    fn op_add_sp_i8(&mut self) {
        let offset = self.fetch_u8() as i8;

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
}

impl Cpu {
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

    fn op_cpl(&mut self) {
        self.registers.register_a = !self.registers.register_a;
        self.status_flags |= STATUS_FLAG_H | STATUS_FLAG_N;
    }

    fn op_scf(&mut self) {
        self.status_flags |= STATUS_FLAG_C;
        self.status_flags &= !(STATUS_FLAG_H | STATUS_FLAG_N);
    }

    fn op_ccf(&mut self) {
        self.status_flags ^= STATUS_FLAG_C;
        self.status_flags &= !(STATUS_FLAG_H | STATUS_FLAG_N);
    }
}
