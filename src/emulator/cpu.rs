use super::memory::Memory;
use paste::paste;

const OP_CODE_FUNCTION_TABLE: [fn(&mut Cpu); 256] = [
    Cpu::op_nop,           // 0x00 : NOP
    Cpu::op_placeholder,   // 0x01 : LD BC,d16
    Cpu::op_load_bc_a,     // 0x02 : LD (BC),A
    Cpu::op_inc_bc,        // 0x03 : INC BC
    Cpu::op_inc_b,         // 0x04 : INC B
    Cpu::op_dec_b,         // 0x05 : DEC B
    Cpu::op_ld_b_u8,       // 0x06 : LD B,d8
    Cpu::op_placeholder,   // 0x07 : RLCA
    Cpu::op_placeholder,   // 0x08 : LD (a16),SP
    Cpu::op_add_hl_bc,     // 0x09 : ADD HL,BC
    Cpu::op_load_a_bc,     // 0x0A : LD A,(BC)
    Cpu::op_dec_bc,        // 0x0B : DEC BC
    Cpu::op_inc_c,         // 0x0C : INC C
    Cpu::op_dec_c,         // 0x0D : DEC C
    Cpu::op_ld_c_u8,       // 0x0E : LD C,d8
    Cpu::op_placeholder,   // 0x0F : RRCA
    Cpu::op_placeholder,   // 0x10 : STOP 0
    Cpu::op_placeholder,   // 0x11 : LD DE,d16
    Cpu::op_load_de_a,     // 0x12 : LD (DE),A
    Cpu::op_inc_de,        // 0x13 : INC DE
    Cpu::op_inc_d,         // 0x14 : INC D
    Cpu::op_dec_d,         // 0x15 : DEC D
    Cpu::op_ld_d_u8,       // 0x16 : LD D,d8
    Cpu::op_placeholder,   // 0x17 : RLA
    Cpu::op_placeholder,   // 0x18 : JR r8
    Cpu::op_add_hl_de,     // 0x19 : ADD HL,DE
    Cpu::op_load_a_de,     // 0x1A : LD A,(DE)
    Cpu::op_dec_de,        // 0x1B : DEC DE
    Cpu::op_inc_e,         // 0x1C : INC E
    Cpu::op_dec_e,         // 0x1D : DEC E
    Cpu::op_ld_e_u8,       // 0x1E : LD E,d8
    Cpu::op_placeholder,   // 0x1F : RRA
    Cpu::op_placeholder,   // 0x20 : JR NZ,r8
    Cpu::op_placeholder,   // 0x21 : LD HL,d16
    Cpu::op_load_hl_inc_a, // 0x22 : LD (HL+),A
    Cpu::op_inc_hl,        // 0x23 : INC HL
    Cpu::op_inc_h,         // 0x24 : INC H
    Cpu::op_dec_h,         // 0x25 : DEC H
    Cpu::op_ld_h_u8,       // 0x26 : LD H,d8
    Cpu::op_daa,           // 0x27 : DAA
    Cpu::op_placeholder,   // 0x28 : JR Z,r8
    Cpu::op_add_hl_hl,     // 0x29 : ADD HL,HL
    Cpu::op_load_a_hl_inc, // 0x2A : LD A,(HL+)
    Cpu::op_dec_hl,        // 0x2B : DEC HL
    Cpu::op_inc_l,         // 0x2C : INC L
    Cpu::op_dec_l,         // 0x2D : DEC L
    Cpu::op_ld_l_u8,       // 0x2E : LD L,d8
    Cpu::op_cpl,           // 0x2F : CPL
    Cpu::op_placeholder,   // 0x30 : JR NC,r8
    Cpu::op_placeholder,   // 0x31 : LD SP,d16
    Cpu::op_load_hl_dec_a, // 0x32 : LD (HL-),A
    Cpu::op_inc_sp,        // 0x33 : INC SP
    Cpu::op_inc_hl_ind,    // 0x34 : INC (HL)
    Cpu::op_dec_hl_ind,    // 0x35 : DEC (HL)
    Cpu::op_load_hl_u8,    // 0x36 : LD (HL),d8
    Cpu::op_scf,           // 0x37 : SCF
    Cpu::op_placeholder,   // 0x38 : JR C,r8
    Cpu::op_add_hl_sp,     // 0x39 : ADD HL,SP
    Cpu::op_load_a_hl_dec, // 0x3A : LD A,(HL-)
    Cpu::op_dec_sp,        // 0x3B : DEC SP
    Cpu::op_inc_a,         // 0x3C : INC A
    Cpu::op_dec_a,         // 0x3D : DEC A
    Cpu::op_ld_a_u8,       // 0x3E : LD A,d8
    Cpu::op_ccf,           // 0x3F : CCF
    Cpu::op_nop,           // 0x40 : LD B,B
    Cpu::op_ld_b_c,        // 0x41 : LD B,C
    Cpu::op_ld_b_d,        // 0x42 : LD B,D
    Cpu::op_ld_b_e,        // 0x43 : LD B,E
    Cpu::op_ld_b_h,        // 0x44 : LD B,H
    Cpu::op_ld_b_l,        // 0x45 : LD B,L
    Cpu::op_load_b_hl,     // 0x46 : LD B,(HL)
    Cpu::op_ld_b_a,        // 0x47 : LD B,A
    Cpu::op_ld_c_b,        // 0x48 : LD C,B
    Cpu::op_nop,           // 0x49 : LD C,C
    Cpu::op_ld_c_d,        // 0x4A : LD C,D
    Cpu::op_ld_c_e,        // 0x4B : LD C,E
    Cpu::op_ld_c_h,        // 0x4C : LD C,H
    Cpu::op_ld_c_l,        // 0x4D : LD C,L
    Cpu::op_load_c_hl,     // 0x4E : LD C,(HL)
    Cpu::op_ld_c_a,        // 0x4F : LD C,A
    Cpu::op_ld_d_b,        // 0x50 : LD D,B
    Cpu::op_ld_d_c,        // 0x51 : LD D,C
    Cpu::op_nop,           // 0x52 : LD D,D
    Cpu::op_ld_d_e,        // 0x53 : LD D,E
    Cpu::op_ld_d_h,        // 0x54 : LD D,H
    Cpu::op_ld_d_l,        // 0x55 : LD D,L
    Cpu::op_load_d_hl,     // 0x56 : LD D,(HL)
    Cpu::op_ld_d_a,        // 0x57 : LD D,A
    Cpu::op_ld_e_b,        // 0x58 : LD E,B
    Cpu::op_ld_e_c,        // 0x59 : LD E,C
    Cpu::op_ld_e_d,        // 0x5A : LD E,D
    Cpu::op_nop,           // 0x5B : LD E,E
    Cpu::op_ld_e_h,        // 0x5C : LD E,H
    Cpu::op_ld_e_l,        // 0x5D : LD E,L
    Cpu::op_load_e_hl,     // 0x5E : LD E,(HL)
    Cpu::op_ld_e_a,        // 0x5F : LD E,A
    Cpu::op_ld_h_b,        // 0x60 : LD H,B
    Cpu::op_ld_h_c,        // 0x61 : LD H,C
    Cpu::op_ld_h_d,        // 0x62 : LD H,D
    Cpu::op_ld_h_e,        // 0x63 : LD H,E
    Cpu::op_nop,           // 0x64 : LD H,H
    Cpu::op_ld_h_l,        // 0x65 : LD H,L
    Cpu::op_load_h_hl,     // 0x66 : LD H,(HL)
    Cpu::op_ld_h_a,        // 0x67 : LD H,A
    Cpu::op_ld_l_b,        // 0x68 : LD L,B
    Cpu::op_ld_l_c,        // 0x69 : LD L,C
    Cpu::op_ld_l_d,        // 0x6A : LD L,D
    Cpu::op_ld_l_e,        // 0x6B : LD L,E
    Cpu::op_ld_l_h,        // 0x6C : LD L,H
    Cpu::op_nop,           // 0x6D : LD L,L
    Cpu::op_load_l_hl,     // 0x6E : LD L,(HL)
    Cpu::op_ld_l_a,        // 0x6F : LD L,A
    Cpu::op_ld_hl_b,       // 0x70 : LD (HL),B
    Cpu::op_ld_hl_c,       // 0x71 : LD (HL),C
    Cpu::op_ld_hl_d,       // 0x72 : LD (HL),D
    Cpu::op_ld_hl_e,       // 0x73 : LD (HL),E
    Cpu::op_ld_hl_h,       // 0x74 : LD (HL),H
    Cpu::op_ld_hl_l,       // 0x75 : LD (HL),L
    Cpu::op_halt,          // 0x76 : HALT
    Cpu::op_ld_hl_a,       // 0x77 : LD (HL),A
    Cpu::op_ld_a_b,        // 0x78 : LD A,B
    Cpu::op_ld_a_c,        // 0x79 : LD A,C
    Cpu::op_ld_a_d,        // 0x7A : LD A,D
    Cpu::op_ld_a_e,        // 0x7B : LD A,E
    Cpu::op_ld_a_h,        // 0x7C : LD A,H
    Cpu::op_ld_a_l,        // 0x7D : LD A,L
    Cpu::op_load_a_hl,     // 0x7E : LD A,(HL)
    Cpu::op_nop,           // 0x7F : LD A,A
    Cpu::op_add_a_b,       // 0x80 : ADD A,B
    Cpu::op_add_a_c,       // 0x81 : ADD A,C
    Cpu::op_add_a_d,       // 0x82 : ADD A,D
    Cpu::op_add_a_e,       // 0x83 : ADD A,E
    Cpu::op_add_a_h,       // 0x84 : ADD A,H
    Cpu::op_add_a_l,       // 0x85 : ADD A,L
    Cpu::op_add_a_hl,      // 0x86 : ADD A,(HL)
    Cpu::op_add_a_a,       // 0x87 : ADD A,A
    Cpu::op_adc_a_b,       // 0x88 : ADC A,B
    Cpu::op_adc_a_c,       // 0x89 : ADC A,C
    Cpu::op_adc_a_d,       // 0x8A : ADC A,D
    Cpu::op_adc_a_e,       // 0x8B : ADC A,E
    Cpu::op_adc_a_h,       // 0x8C : ADC A,H
    Cpu::op_adc_a_l,       // 0x8D : ADC A,L
    Cpu::op_adc_a_hl,      // 0x8E : ADC A,(HL)
    Cpu::op_adc_a_a,       // 0x8F : ADC A,A
    Cpu::op_sub_a_b,       // 0x90 : SUB B
    Cpu::op_sub_a_c,       // 0x91 : SUB C
    Cpu::op_sub_a_d,       // 0x92 : SUB D
    Cpu::op_sub_a_e,       // 0x93 : SUB E
    Cpu::op_sub_a_h,       // 0x94 : SUB H
    Cpu::op_sub_a_l,       // 0x95 : SUB L
    Cpu::op_sub_a_hl,      // 0x96 : SUB (HL)
    Cpu::op_sub_a_a,       // 0x97 : SUB A
    Cpu::op_sbc_a_b,       // 0x98 : SBC A,B
    Cpu::op_sbc_a_c,       // 0x99 : SBC A,C
    Cpu::op_sbc_a_d,       // 0x9A : SBC A,D
    Cpu::op_sbc_a_e,       // 0x9B : SBC A,E
    Cpu::op_sbc_a_h,       // 0x9C : SBC A,H
    Cpu::op_sbc_a_l,       // 0x9D : SBC A,L
    Cpu::op_sbc_a_hl,      // 0x9E : SBC A,(HL)
    Cpu::op_sbc_a_a,       // 0x9F : SBC A,A
    Cpu::op_and_a_b,       // 0xA0 : AND B
    Cpu::op_and_a_c,       // 0xA1 : AND C
    Cpu::op_and_a_d,       // 0xA2 : AND D
    Cpu::op_and_a_e,       // 0xA3 : AND E
    Cpu::op_and_a_h,       // 0xA4 : AND H
    Cpu::op_and_a_l,       // 0xA5 : AND L
    Cpu::op_and_a_hl,      // 0xA6 : AND (HL)
    Cpu::op_and_a_a,       // 0xA7 : AND A
    Cpu::op_xor_a_b,       // 0xA8 : XOR B
    Cpu::op_xor_a_c,       // 0xA9 : XOR C
    Cpu::op_xor_a_d,       // 0xAA : XOR D
    Cpu::op_xor_a_e,       // 0xAB : XOR E
    Cpu::op_xor_a_h,       // 0xAC : XOR H
    Cpu::op_xor_a_l,       // 0xAD : XOR L
    Cpu::op_xor_a_hl,      // 0xAE : XOR (HL)
    Cpu::op_xor_a_a,       // 0xAF : XOR A
    Cpu::op_or_a_b,        // 0xB0 : OR B
    Cpu::op_or_a_c,        // 0xB1 : OR C
    Cpu::op_or_a_d,        // 0xB2 : OR D
    Cpu::op_or_a_e,        // 0xB3 : OR E
    Cpu::op_or_a_h,        // 0xB4 : OR H
    Cpu::op_or_a_l,        // 0xB5 : OR L
    Cpu::op_or_a_hl,       // 0xB6 : OR (HL)
    Cpu::op_or_a_a,        // 0xB7 : OR A
    Cpu::op_cp_a_b,        // 0xB8 : CP B
    Cpu::op_cp_a_c,        // 0xB9 : CP C
    Cpu::op_cp_a_d,        // 0xBA : CP D
    Cpu::op_cp_a_e,        // 0xBB : CP E
    Cpu::op_cp_a_h,        // 0xBC : CP H
    Cpu::op_cp_a_l,        // 0xBD : CP L
    Cpu::op_cp_a_hl,       // 0xBE : CP (HL)
    Cpu::op_cp_a_a,        // 0xBF : CP A
    Cpu::op_placeholder,   // 0xC0 : RET NZ
    Cpu::op_placeholder,   // 0xC1 : POP BC
    Cpu::op_placeholder,   // 0xC2 : JP NZ,a16
    Cpu::op_placeholder,   // 0xC3 : JP a16
    Cpu::op_placeholder,   // 0xC4 : CALL NZ,a16
    Cpu::op_placeholder,   // 0xC5 : PUSH BC
    Cpu::op_add_a_u8,      // 0xC6 : ADD A,d8
    Cpu::op_placeholder,   // 0xC7 : RST 00H
    Cpu::op_placeholder,   // 0xC8 : RET Z
    Cpu::op_placeholder,   // 0xC9 : RET
    Cpu::op_placeholder,   // 0xCA : JP Z,a16
    Cpu::op_placeholder,   // 0xCB : PREFIX CB
    Cpu::op_placeholder,   // 0xCC : CALL Z,a16
    Cpu::op_placeholder,   // 0xCD : CALL a16
    Cpu::op_adc_a_u8,      // 0xCE : ADC A,d8
    Cpu::op_placeholder,   // 0xCF : RST 08H
    Cpu::op_placeholder,   // 0xD0 : RET NC
    Cpu::op_placeholder,   // 0xD1 : POP DE
    Cpu::op_placeholder,   // 0xD2 : JP NC,a16
    Cpu::op_placeholder,   // 0xD3 : undefined
    Cpu::op_placeholder,   // 0xD4 : CALL NC,a16
    Cpu::op_placeholder,   // 0xD5 : PUSH DE
    Cpu::op_sub_a_u8,      // 0xD6 : SUB d8
    Cpu::op_placeholder,   // 0xD7 : RST 10H
    Cpu::op_placeholder,   // 0xD8 : RET C
    Cpu::op_placeholder,   // 0xD9 : RETI
    Cpu::op_placeholder,   // 0xDA : JP C,a16
    Cpu::op_placeholder,   // 0xDB : undefined
    Cpu::op_placeholder,   // 0xDC : CALL C,a16
    Cpu::op_placeholder,   // 0xDD : undefined
    Cpu::op_sbc_a_u8,      // 0xDE : SBC A,d8
    Cpu::op_placeholder,   // 0xDF : RST 18H
    Cpu::op_placeholder,   // 0xE0 : LDH (a8),A
    Cpu::op_placeholder,   // 0xE1 : POP HL
    Cpu::op_placeholder,   // 0xE2 : LD (C),A
    Cpu::op_placeholder,   // 0xE3 : undefined
    Cpu::op_placeholder,   // 0xE4 : undefined
    Cpu::op_placeholder,   // 0xE5 : PUSH HL
    Cpu::op_and_a_u8,      // 0xE6 : AND d8
    Cpu::op_placeholder,   // 0xE7 : RST 20H
    Cpu::op_add_sp_i8,     // 0xE8 : ADD SP,r8
    Cpu::op_placeholder,   // 0xE9 : JP (HL)
    Cpu::op_placeholder,   // 0xEA : LD (a16),A
    Cpu::op_placeholder,   // 0xEB : undefined
    Cpu::op_placeholder,   // 0xEC : undefined
    Cpu::op_placeholder,   // 0xED : undefined
    Cpu::op_xor_a_u8,      // 0xEE : XOR d8
    Cpu::op_placeholder,   // 0xEF : RST 28H
    Cpu::op_placeholder,   // 0xF0 : LDH A,(a8)
    Cpu::op_placeholder,   // 0xF1 : POP AF
    Cpu::op_placeholder,   // 0xF2 : LD A,(C)
    Cpu::op_placeholder,   // 0xF3 : DI
    Cpu::op_placeholder,   // 0xF4 : undefined
    Cpu::op_placeholder,   // 0xF5 : PUSH AF
    Cpu::op_or_a_u8,       // 0xF6 : OR d8
    Cpu::op_placeholder,   // 0xF7 : RST 30H
    Cpu::op_placeholder,   // 0xF8 : LD HL,SP+r8
    Cpu::op_placeholder,   // 0xF9 : LD SP,HL
    Cpu::op_placeholder,   // 0xFA : LD A,(a16)
    Cpu::op_placeholder,   // 0xFB : EI
    Cpu::op_placeholder,   // 0xFC : undefined
    Cpu::op_placeholder,   // 0xFD : undefined
    Cpu::op_cp_a_u8,       // 0xFE : CP d8
    Cpu::op_placeholder,   // 0xFF : RST 38H
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
        OP_CODE_FUNCTION_TABLE[self.fetch_next_byte() as usize](self);
    }

    fn op_nop(&mut self) {}

    fn op_load_bc_a(&mut self) {
        self.memory
            .write(self.registers.bc(), self.registers.register_a);
    }

    fn op_load_a_bc(&mut self) {
        self.registers.register_a = self.memory.read(self.registers.bc());
    }

    fn op_load_de_a(&mut self) {
        self.memory
            .write(self.registers.de(), self.registers.register_a);
    }

    fn op_load_a_de(&mut self) {
        self.registers.register_a = self.memory.read(self.registers.de());
    }

    fn op_load_a_hl_dec(&mut self) {
        let address = self.registers.hl();
        self.registers.register_a = self.memory.read(address);
        self.registers.set_hl(address - 1);
    }

    fn op_load_hl_dec_a(&mut self) {
        let address = self.registers.hl();
        self.memory.write(address, self.registers.register_a);
        self.registers.set_hl(address - 1);
    }

    fn op_load_a_hl_inc(&mut self) {
        let address = self.registers.hl();
        self.registers.register_a = self.memory.read(address);
        self.registers.set_hl(address + 1);
    }

    fn op_load_hl_inc_a(&mut self) {
        let address = self.registers.hl();
        self.memory.write(address, self.registers.register_a);
        self.registers.set_hl(address + 1);
    }

    fn op_load_hl_u8(&mut self) {
        let value = self.fetch_next_byte();
        self.memory.write(self.registers.hl(), value);
    }

    fn op_halt(&mut self) {
        self.op_placeholder();
    }

    fn op_placeholder(&mut self) {
        panic!("Opcode not implemented!")
    }

    fn fetch_next_byte(&mut self) -> u8 {
        let value = self.memory.read(self.program_counter);
        self.program_counter += 1;

        value
    }
}

impl Cpu {
    fn op_ld_a_u8(&mut self) {
        self.registers.register_a = self.fetch_next_byte();
    }

    fn op_ld_b_u8(&mut self) {
        self.registers.register_b = self.fetch_next_byte();
    }

    fn op_ld_c_u8(&mut self) {
        self.registers.register_c = self.fetch_next_byte();
    }

    fn op_ld_d_u8(&mut self) {
        self.registers.register_d = self.fetch_next_byte();
    }

    fn op_ld_e_u8(&mut self) {
        self.registers.register_e = self.fetch_next_byte();
    }

    fn op_ld_h_u8(&mut self) {
        self.registers.register_h = self.fetch_next_byte();
    }

    fn op_ld_l_u8(&mut self) {
        self.registers.register_l = self.fetch_next_byte();
    }
}

impl Cpu {
    fn op_ld_a_b(&mut self) {
        self.registers.register_a = self.registers.register_b;
    }

    fn op_ld_a_c(&mut self) {
        self.registers.register_a = self.registers.register_c;
    }

    fn op_ld_a_d(&mut self) {
        self.registers.register_a = self.registers.register_d;
    }

    fn op_ld_a_e(&mut self) {
        self.registers.register_a = self.registers.register_e;
    }

    fn op_ld_a_h(&mut self) {
        self.registers.register_a = self.registers.register_h;
    }

    fn op_ld_a_l(&mut self) {
        self.registers.register_a = self.registers.register_l;
    }

    fn op_ld_b_a(&mut self) {
        self.registers.register_b = self.registers.register_a;
    }

    fn op_ld_b_c(&mut self) {
        self.registers.register_b = self.registers.register_c;
    }

    fn op_ld_b_d(&mut self) {
        self.registers.register_b = self.registers.register_d;
    }

    fn op_ld_b_e(&mut self) {
        self.registers.register_b = self.registers.register_e;
    }

    fn op_ld_b_h(&mut self) {
        self.registers.register_b = self.registers.register_h;
    }

    fn op_ld_b_l(&mut self) {
        self.registers.register_b = self.registers.register_l;
    }

    fn op_ld_c_a(&mut self) {
        self.registers.register_c = self.registers.register_a;
    }

    fn op_ld_c_b(&mut self) {
        self.registers.register_c = self.registers.register_b;
    }

    fn op_ld_c_d(&mut self) {
        self.registers.register_c = self.registers.register_d;
    }

    fn op_ld_c_e(&mut self) {
        self.registers.register_c = self.registers.register_e;
    }

    fn op_ld_c_h(&mut self) {
        self.registers.register_c = self.registers.register_h;
    }

    fn op_ld_c_l(&mut self) {
        self.registers.register_c = self.registers.register_l;
    }

    fn op_ld_d_a(&mut self) {
        self.registers.register_d = self.registers.register_a;
    }

    fn op_ld_d_b(&mut self) {
        self.registers.register_d = self.registers.register_b;
    }

    fn op_ld_d_c(&mut self) {
        self.registers.register_d = self.registers.register_c;
    }

    fn op_ld_d_e(&mut self) {
        self.registers.register_d = self.registers.register_e;
    }

    fn op_ld_d_h(&mut self) {
        self.registers.register_d = self.registers.register_h;
    }

    fn op_ld_d_l(&mut self) {
        self.registers.register_d = self.registers.register_l;
    }

    fn op_ld_e_a(&mut self) {
        self.registers.register_e = self.registers.register_a;
    }

    fn op_ld_e_b(&mut self) {
        self.registers.register_e = self.registers.register_b;
    }

    fn op_ld_e_c(&mut self) {
        self.registers.register_e = self.registers.register_c;
    }

    fn op_ld_e_d(&mut self) {
        self.registers.register_e = self.registers.register_d;
    }

    fn op_ld_e_h(&mut self) {
        self.registers.register_e = self.registers.register_h;
    }

    fn op_ld_e_l(&mut self) {
        self.registers.register_e = self.registers.register_l;
    }

    fn op_ld_h_a(&mut self) {
        self.registers.register_h = self.registers.register_a;
    }

    fn op_ld_h_b(&mut self) {
        self.registers.register_h = self.registers.register_b;
    }

    fn op_ld_h_c(&mut self) {
        self.registers.register_h = self.registers.register_c;
    }

    fn op_ld_h_d(&mut self) {
        self.registers.register_h = self.registers.register_d;
    }

    fn op_ld_h_e(&mut self) {
        self.registers.register_h = self.registers.register_e;
    }

    fn op_ld_h_l(&mut self) {
        self.registers.register_h = self.registers.register_l;
    }

    fn op_ld_l_a(&mut self) {
        self.registers.register_l = self.registers.register_a;
    }

    fn op_ld_l_b(&mut self) {
        self.registers.register_l = self.registers.register_b;
    }

    fn op_ld_l_c(&mut self) {
        self.registers.register_l = self.registers.register_c;
    }

    fn op_ld_l_d(&mut self) {
        self.registers.register_l = self.registers.register_d;
    }

    fn op_ld_l_e(&mut self) {
        self.registers.register_l = self.registers.register_e;
    }

    fn op_ld_l_h(&mut self) {
        self.registers.register_l = self.registers.register_h;
    }
}

impl Cpu {
    fn op_ld_hl(&mut self, value: u8) {
        self.memory.write(self.registers.hl(), value);
    }

    fn op_ld_hl_a(&mut self) {
        self.op_ld_hl(self.registers.register_a);
    }

    fn op_ld_hl_b(&mut self) {
        self.op_ld_hl(self.registers.register_b);
    }

    fn op_ld_hl_c(&mut self) {
        self.op_ld_hl(self.registers.register_c);
    }

    fn op_ld_hl_d(&mut self) {
        self.op_ld_hl(self.registers.register_d);
    }

    fn op_ld_hl_e(&mut self) {
        self.op_ld_hl(self.registers.register_e);
    }

    fn op_ld_hl_h(&mut self) {
        self.op_ld_hl(self.registers.register_h);
    }

    fn op_ld_hl_l(&mut self) {
        self.op_ld_hl(self.registers.register_l);
    }
}

macro_rules! op_load_r_hl {
    ($x:tt) => {
        paste! {
            fn [< op_load_ $x _hl>] (&mut self) {
                self.registers.[< register_ $x >] = self.memory
                    .read(self.registers.hl());
            }
        }
    };
}

impl Cpu {
    op_load_r_hl!(b);
    op_load_r_hl!(c);
    op_load_r_hl!(d);
    op_load_r_hl!(e);
    op_load_r_hl!(h);
    op_load_r_hl!(l);
    op_load_r_hl!(a);
}

impl Cpu {
    fn op_add_a(&mut self, operand: u8) {
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

    fn op_add_a_a(&mut self) {
        self.op_add_a(self.registers.register_a);
    }

    fn op_add_a_b(&mut self) {
        self.op_add_a(self.registers.register_b);
    }

    fn op_add_a_c(&mut self) {
        self.op_add_a(self.registers.register_c);
    }

    fn op_add_a_d(&mut self) {
        self.op_add_a(self.registers.register_d);
    }

    fn op_add_a_e(&mut self) {
        self.op_add_a(self.registers.register_e);
    }

    fn op_add_a_h(&mut self) {
        self.op_add_a(self.registers.register_h);
    }

    fn op_add_a_l(&mut self) {
        self.op_add_a(self.registers.register_l);
    }

    fn op_add_a_hl(&mut self) {
        self.op_add_a(self.memory.read(self.registers.hl()));
    }

    fn op_add_a_u8(&mut self) {
        let operand = self.fetch_next_byte();
        self.op_add_a(operand);
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
        let offset = self.fetch_next_byte() as i8;

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
    fn op_adc_a(&mut self, operand: u8) {
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

    fn op_adc_a_a(&mut self) {
        self.op_adc_a(self.registers.register_a);
    }

    fn op_adc_a_b(&mut self) {
        self.op_adc_a(self.registers.register_b);
    }

    fn op_adc_a_c(&mut self) {
        self.op_adc_a(self.registers.register_c);
    }

    fn op_adc_a_d(&mut self) {
        self.op_adc_a(self.registers.register_d);
    }

    fn op_adc_a_e(&mut self) {
        self.op_adc_a(self.registers.register_e);
    }

    fn op_adc_a_h(&mut self) {
        self.op_adc_a(self.registers.register_h);
    }

    fn op_adc_a_l(&mut self) {
        self.op_adc_a(self.registers.register_l);
    }

    fn op_adc_a_hl(&mut self) {
        self.op_adc_a(self.memory.read(self.registers.hl()));
    }

    fn op_adc_a_u8(&mut self) {
        let operand = self.fetch_next_byte();
        self.op_adc_a(operand);
    }
}

impl Cpu {
    fn op_sub_a(&mut self, operand: u8) {
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

    fn op_sub_a_a(&mut self) {
        self.op_sub_a(self.registers.register_a);
    }

    fn op_sub_a_b(&mut self) {
        self.op_sub_a(self.registers.register_b);
    }

    fn op_sub_a_c(&mut self) {
        self.op_sub_a(self.registers.register_c);
    }

    fn op_sub_a_d(&mut self) {
        self.op_sub_a(self.registers.register_d);
    }

    fn op_sub_a_e(&mut self) {
        self.op_sub_a(self.registers.register_e);
    }

    fn op_sub_a_h(&mut self) {
        self.op_sub_a(self.registers.register_h);
    }

    fn op_sub_a_l(&mut self) {
        self.op_sub_a(self.registers.register_l);
    }

    fn op_sub_a_hl(&mut self) {
        self.op_sub_a(self.memory.read(self.registers.hl()));
    }

    fn op_sub_a_u8(&mut self) {
        let operand = self.fetch_next_byte();
        self.op_sub_a(operand);
    }
}

impl Cpu {
    fn op_sbc_a(&mut self, operand: u8) {
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

    fn op_sbc_a_a(&mut self) {
        self.op_sbc_a(self.registers.register_a);
    }

    fn op_sbc_a_b(&mut self) {
        self.op_sbc_a(self.registers.register_b);
    }

    fn op_sbc_a_c(&mut self) {
        self.op_sbc_a(self.registers.register_c);
    }

    fn op_sbc_a_d(&mut self) {
        self.op_sbc_a(self.registers.register_d);
    }

    fn op_sbc_a_e(&mut self) {
        self.op_sbc_a(self.registers.register_e);
    }

    fn op_sbc_a_h(&mut self) {
        self.op_sbc_a(self.registers.register_h);
    }

    fn op_sbc_a_l(&mut self) {
        self.op_sbc_a(self.registers.register_l);
    }

    fn op_sbc_a_hl(&mut self) {
        self.op_sbc_a(self.memory.read(self.registers.hl()));
    }

    fn op_sbc_a_u8(&mut self) {
        let operand = self.fetch_next_byte();
        self.op_sbc_a(operand);
    }
}

impl Cpu {
    fn op_and_a(&mut self, operand: u8) {
        self.registers.register_a &= operand;
        self.status_flags = STATUS_FLAG_H;

        if self.registers.register_a == 0 {
            self.status_flags |= STATUS_FLAG_Z;
        }
    }

    fn op_and_a_a(&mut self) {
        self.op_and_a(self.registers.register_a);
    }

    fn op_and_a_b(&mut self) {
        self.op_and_a(self.registers.register_b);
    }

    fn op_and_a_c(&mut self) {
        self.op_and_a(self.registers.register_c);
    }

    fn op_and_a_d(&mut self) {
        self.op_and_a(self.registers.register_d);
    }

    fn op_and_a_e(&mut self) {
        self.op_and_a(self.registers.register_e);
    }

    fn op_and_a_h(&mut self) {
        self.op_and_a(self.registers.register_h);
    }

    fn op_and_a_l(&mut self) {
        self.op_and_a(self.registers.register_l);
    }

    fn op_and_a_hl(&mut self) {
        self.op_and_a(self.memory.read(self.registers.hl()));
    }

    fn op_and_a_u8(&mut self) {
        let operand = self.fetch_next_byte();
        self.op_and_a(operand);
    }
}

impl Cpu {
    fn op_xor_a(&mut self, operand: u8) {
        self.registers.register_a ^= operand;
        self.status_flags = STATUS_FLAG_H;

        if self.registers.register_a == 0 {
            self.status_flags |= STATUS_FLAG_Z;
        }
    }

    fn op_xor_a_a(&mut self) {
        self.op_xor_a(self.registers.register_a);
    }

    fn op_xor_a_b(&mut self) {
        self.op_xor_a(self.registers.register_b);
    }

    fn op_xor_a_c(&mut self) {
        self.op_xor_a(self.registers.register_c);
    }

    fn op_xor_a_d(&mut self) {
        self.op_xor_a(self.registers.register_d);
    }

    fn op_xor_a_e(&mut self) {
        self.op_xor_a(self.registers.register_e);
    }

    fn op_xor_a_h(&mut self) {
        self.op_xor_a(self.registers.register_h);
    }

    fn op_xor_a_l(&mut self) {
        self.op_xor_a(self.registers.register_l);
    }

    fn op_xor_a_hl(&mut self) {
        self.op_xor_a(self.memory.read(self.registers.hl()));
    }

    fn op_xor_a_u8(&mut self) {
        let operand = self.fetch_next_byte();
        self.op_xor_a(operand);
    }
}

impl Cpu {
    fn op_or_a(&mut self, operand: u8) {
        self.registers.register_a |= operand;
        self.status_flags = STATUS_FLAG_H;

        if self.registers.register_a == 0 {
            self.status_flags |= STATUS_FLAG_Z;
        }
    }

    fn op_or_a_a(&mut self) {
        self.op_or_a(self.registers.register_a);
    }

    fn op_or_a_b(&mut self) {
        self.op_or_a(self.registers.register_b);
    }

    fn op_or_a_c(&mut self) {
        self.op_or_a(self.registers.register_c);
    }

    fn op_or_a_d(&mut self) {
        self.op_or_a(self.registers.register_d);
    }

    fn op_or_a_e(&mut self) {
        self.op_or_a(self.registers.register_e);
    }

    fn op_or_a_h(&mut self) {
        self.op_or_a(self.registers.register_h);
    }

    fn op_or_a_l(&mut self) {
        self.op_or_a(self.registers.register_l);
    }

    fn op_or_a_hl(&mut self) {
        self.op_or_a(self.memory.read(self.registers.hl()));
    }

    fn op_or_a_u8(&mut self) {
        let operand = self.fetch_next_byte();
        self.op_or_a(operand);
    }
}

impl Cpu {
    fn op_cp_a(&mut self, operand: u8) {
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

    fn op_cp_a_a(&mut self) {
        self.op_cp_a(self.registers.register_a);
    }

    fn op_cp_a_b(&mut self) {
        self.op_cp_a(self.registers.register_b);
    }

    fn op_cp_a_c(&mut self) {
        self.op_cp_a(self.registers.register_c);
    }

    fn op_cp_a_d(&mut self) {
        self.op_cp_a(self.registers.register_d);
    }

    fn op_cp_a_e(&mut self) {
        self.op_cp_a(self.registers.register_e);
    }

    fn op_cp_a_h(&mut self) {
        self.op_cp_a(self.registers.register_h);
    }

    fn op_cp_a_l(&mut self) {
        self.op_cp_a(self.registers.register_l);
    }

    fn op_cp_a_hl(&mut self) {
        self.op_cp_a(self.memory.read(self.registers.hl()));
    }

    fn op_cp_a_u8(&mut self) {
        let operand = self.fetch_next_byte();
        self.op_cp_a(operand);
    }
}

impl Cpu {
    fn op_inc_u8(&mut self, operand: u8) -> u8 {
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

    fn op_inc_a(&mut self) {
        self.registers.register_a = self.op_inc_u8(self.registers.register_a);
    }

    fn op_inc_b(&mut self) {
        self.registers.register_b = self.op_inc_u8(self.registers.register_b);
    }

    fn op_inc_c(&mut self) {
        self.registers.register_c = self.op_inc_u8(self.registers.register_c);
    }

    fn op_inc_d(&mut self) {
        self.registers.register_d = self.op_inc_u8(self.registers.register_d);
    }

    fn op_inc_e(&mut self) {
        self.registers.register_e = self.op_inc_u8(self.registers.register_e);
    }

    fn op_inc_h(&mut self) {
        self.registers.register_h = self.op_inc_u8(self.registers.register_h);
    }

    fn op_inc_l(&mut self) {
        self.registers.register_l = self.op_inc_u8(self.registers.register_l);
    }

    fn op_inc_hl_ind(&mut self) {
        let address = self.registers.hl();
        let value = self.op_inc_u8(self.memory.read(address));
        self.memory.write(address, value);
    }
}

impl Cpu {
    fn op_inc_bc(&mut self) {
        self.registers.set_bc(self.registers.bc().wrapping_add(1));
    }

    fn op_inc_de(&mut self) {
        self.registers.set_de(self.registers.de().wrapping_add(1));
    }

    fn op_inc_hl(&mut self) {
        self.registers.set_hl(self.registers.hl().wrapping_add(1));
    }

    fn op_inc_sp(&mut self) {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
    }
}

impl Cpu {
    fn op_dec(&mut self, operand: u8) -> u8 {
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

    fn op_dec_a(&mut self) {
        self.registers.register_a = self.op_dec(self.registers.register_a);
    }

    fn op_dec_b(&mut self) {
        self.registers.register_b = self.op_dec(self.registers.register_b);
    }

    fn op_dec_c(&mut self) {
        self.registers.register_c = self.op_dec(self.registers.register_c);
    }

    fn op_dec_d(&mut self) {
        self.registers.register_d = self.op_dec(self.registers.register_d);
    }

    fn op_dec_e(&mut self) {
        self.registers.register_e = self.op_dec(self.registers.register_e);
    }

    fn op_dec_h(&mut self) {
        self.registers.register_h = self.op_dec(self.registers.register_h);
    }

    fn op_dec_l(&mut self) {
        self.registers.register_l = self.op_dec(self.registers.register_l);
    }

    fn op_dec_hl_ind(&mut self) {
        let address = self.registers.hl();
        let value = self.op_dec(self.memory.read(address));
        self.memory.write(address, value);
    }
}

impl Cpu {
    fn op_dec_bc(&mut self) {
        self.registers.set_bc(self.registers.bc().wrapping_sub(1));
    }

    fn op_dec_de(&mut self) {
        self.registers.set_de(self.registers.de().wrapping_sub(1));
    }

    fn op_dec_hl(&mut self) {
        self.registers.set_hl(self.registers.hl().wrapping_sub(1));
    }

    fn op_dec_sp(&mut self) {
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
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
