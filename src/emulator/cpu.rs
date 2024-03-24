use super::memory::Memory;
use paste::paste;

const OP_CODE_FUNCTION_TABLE: [fn(&mut Cpu); 256] = [
    Cpu::op_nop,           // 0x00 : NOP
    Cpu::op_placeholder,   // 0x01 : LD BC,d16
    Cpu::op_load_bc_a,     // 0x02 : LD (BC),A
    Cpu::op_placeholder,   // 0x03 : INC BC
    Cpu::op_placeholder,   // 0x04 : INC B
    Cpu::op_placeholder,   // 0x05 : DEC B
    Cpu::op_load_b_u8,     // 0x06 : LD B,d8
    Cpu::op_placeholder,   // 0x07 : RLCA
    Cpu::op_placeholder,   // 0x08 : LD (a16),SP
    Cpu::op_placeholder,   // 0x09 : ADD HL,BC
    Cpu::op_load_a_bc,     // 0x0A : LD A,(BC)
    Cpu::op_placeholder,   // 0x0B : DEC BC
    Cpu::op_placeholder,   // 0x0C : INC C
    Cpu::op_placeholder,   // 0x0D : DEC C
    Cpu::op_load_c_u8,     // 0x0E : LD C,d8
    Cpu::op_placeholder,   // 0x0F : RRCA
    Cpu::op_placeholder,   // 0x10 : STOP 0
    Cpu::op_placeholder,   // 0x11 : LD DE,d16
    Cpu::op_load_de_a,     // 0x12 : LD (DE),A
    Cpu::op_placeholder,   // 0x13 : INC DE
    Cpu::op_placeholder,   // 0x14 : INC D
    Cpu::op_placeholder,   // 0x15 : DEC D
    Cpu::op_load_d_u8,     // 0x16 : LD D,d8
    Cpu::op_placeholder,   // 0x17 : RLA
    Cpu::op_placeholder,   // 0x18 : JR r8
    Cpu::op_placeholder,   // 0x19 : ADD HL,DE
    Cpu::op_load_a_de,     // 0x1A : LD A,(DE)
    Cpu::op_placeholder,   // 0x1B : DEC DE
    Cpu::op_placeholder,   // 0x1C : INC E
    Cpu::op_placeholder,   // 0x1D : DEC E
    Cpu::op_load_e_u8,     // 0x1E : LD E,d8
    Cpu::op_placeholder,   // 0x1F : RRA
    Cpu::op_placeholder,   // 0x20 : JR NZ,r8
    Cpu::op_placeholder,   // 0x21 : LD HL,d16
    Cpu::op_load_hl_inc_a, // 0x22 : LD (HL+),A
    Cpu::op_placeholder,   // 0x23 : INC HL
    Cpu::op_placeholder,   // 0x24 : INC H
    Cpu::op_placeholder,   // 0x25 : DEC H
    Cpu::op_load_h_u8,     // 0x26 : LD H,d8
    Cpu::op_placeholder,   // 0x27 : DAA
    Cpu::op_placeholder,   // 0x28 : JR Z,r8
    Cpu::op_placeholder,   // 0x29 : ADD HL,HL
    Cpu::op_load_a_hl_inc, // 0x2A : LD A,(HL+)
    Cpu::op_placeholder,   // 0x2B : DEC HL
    Cpu::op_placeholder,   // 0x2C : INC L
    Cpu::op_placeholder,   // 0x2D : DEC L
    Cpu::op_load_l_u8,     // 0x2E : LD L,d8
    Cpu::op_placeholder,   // 0x2F : CPL
    Cpu::op_placeholder,   // 0x30 : JR NC,r8
    Cpu::op_placeholder,   // 0x31 : LD SP,d16
    Cpu::op_load_hl_dec_a, // 0x32 : LD (HL-),A
    Cpu::op_placeholder,   // 0x33 : INC SP
    Cpu::op_placeholder,   // 0x34 : INC (HL)
    Cpu::op_placeholder,   // 0x35 : DEC (HL)
    Cpu::op_load_hl_u8,    // 0x36 : LD (HL),d8
    Cpu::op_placeholder,   // 0x37 : SCF
    Cpu::op_placeholder,   // 0x38 : JR C,r8
    Cpu::op_placeholder,   // 0x39 : ADD HL,SP
    Cpu::op_load_a_hl_dec, // 0x3A : LD A,(HL-)
    Cpu::op_placeholder,   // 0x3B : DEC SP
    Cpu::op_placeholder,   // 0x3C : INC A
    Cpu::op_placeholder,   // 0x3D : DEC A
    Cpu::op_load_a_u8,     // 0x3E : LD A,d8
    Cpu::op_placeholder,   // 0x3F : CCF
    Cpu::op_load_b_b,      // 0x40 : LD B,B
    Cpu::op_load_b_c,      // 0x41 : LD B,C
    Cpu::op_load_b_d,      // 0x42 : LD B,D
    Cpu::op_load_b_e,      // 0x43 : LD B,E
    Cpu::op_load_b_h,      // 0x44 : LD B,H
    Cpu::op_load_b_l,      // 0x45 : LD B,L
    Cpu::op_load_b_hl,     // 0x46 : LD B,(HL)
    Cpu::op_load_b_a,      // 0x47 : LD B,A
    Cpu::op_load_c_b,      // 0x48 : LD C,B
    Cpu::op_load_c_c,      // 0x49 : LD C,C
    Cpu::op_load_c_d,      // 0x4A : LD C,D
    Cpu::op_load_c_e,      // 0x4B : LD C,E
    Cpu::op_load_c_h,      // 0x4C : LD C,H
    Cpu::op_load_c_l,      // 0x4D : LD C,L
    Cpu::op_load_c_hl,     // 0x4E : LD C,(HL)
    Cpu::op_load_c_a,      // 0x4F : LD C,A
    Cpu::op_load_d_b,      // 0x50 : LD D,B
    Cpu::op_load_d_c,      // 0x51 : LD D,C
    Cpu::op_load_d_d,      // 0x52 : LD D,D
    Cpu::op_load_d_e,      // 0x53 : LD D,E
    Cpu::op_load_d_h,      // 0x54 : LD D,H
    Cpu::op_load_d_l,      // 0x55 : LD D,L
    Cpu::op_load_d_hl,     // 0x56 : LD D,(HL)
    Cpu::op_load_d_a,      // 0x57 : LD D,A
    Cpu::op_load_e_b,      // 0x58 : LD E,B
    Cpu::op_load_e_c,      // 0x59 : LD E,C
    Cpu::op_load_e_d,      // 0x5A : LD E,D
    Cpu::op_load_e_e,      // 0x5B : LD E,E
    Cpu::op_load_e_h,      // 0x5C : LD E,H
    Cpu::op_load_e_l,      // 0x5D : LD E,L
    Cpu::op_load_e_hl,     // 0x5E : LD E,(HL)
    Cpu::op_load_e_a,      // 0x5F : LD E,A
    Cpu::op_load_h_b,      // 0x60 : LD H,B
    Cpu::op_load_h_c,      // 0x61 : LD H,C
    Cpu::op_load_h_d,      // 0x62 : LD H,D
    Cpu::op_load_h_e,      // 0x63 : LD H,E
    Cpu::op_load_h_h,      // 0x64 : LD H,H
    Cpu::op_load_h_l,      // 0x65 : LD H,L
    Cpu::op_load_h_hl,     // 0x66 : LD H,(HL)
    Cpu::op_load_h_a,      // 0x67 : LD H,A
    Cpu::op_load_l_b,      // 0x68 : LD L,B
    Cpu::op_load_l_c,      // 0x69 : LD L,C
    Cpu::op_load_l_d,      // 0x6A : LD L,D
    Cpu::op_load_l_e,      // 0x6B : LD L,E
    Cpu::op_load_l_h,      // 0x6C : LD L,H
    Cpu::op_load_l_l,      // 0x6D : LD L,L
    Cpu::op_load_l_hl,     // 0x6E : LD L,(HL)
    Cpu::op_load_l_a,      // 0x6F : LD L,A
    Cpu::op_load_hl_b,     // 0x70 : LD (HL),B
    Cpu::op_load_hl_c,     // 0x71 : LD (HL),C
    Cpu::op_load_hl_d,     // 0x72 : LD (HL),D
    Cpu::op_load_hl_e,     // 0x73 : LD (HL),E
    Cpu::op_load_hl_h,     // 0x74 : LD (HL),H
    Cpu::op_load_hl_l,     // 0x75 : LD (HL),L
    Cpu::op_halt,          // 0x76 : HALT
    Cpu::op_load_hl_a,     // 0x77 : LD (HL),A
    Cpu::op_load_a_b,      // 0x78 : LD A,B
    Cpu::op_load_a_c,      // 0x79 : LD A,C
    Cpu::op_load_a_d,      // 0x7A : LD A,D
    Cpu::op_load_a_e,      // 0x7B : LD A,E
    Cpu::op_load_a_h,      // 0x7C : LD A,H
    Cpu::op_load_a_l,      // 0x7D : LD A,L
    Cpu::op_load_a_hl,     // 0x7E : LD A,(HL)
    Cpu::op_load_a_a,      // 0x7F : LD A,A
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
    Cpu::op_placeholder,   // 0xB8 : CP B
    Cpu::op_placeholder,   // 0xB9 : CP C
    Cpu::op_placeholder,   // 0xBA : CP D
    Cpu::op_placeholder,   // 0xBB : CP E
    Cpu::op_placeholder,   // 0xBC : CP H
    Cpu::op_placeholder,   // 0xBD : CP L
    Cpu::op_placeholder,   // 0xBE : CP (HL)
    Cpu::op_placeholder,   // 0xBF : CP A
    Cpu::op_placeholder,   // 0xC0 : RET NZ
    Cpu::op_placeholder,   // 0xC1 : POP BC
    Cpu::op_placeholder,   // 0xC2 : JP NZ,a16
    Cpu::op_placeholder,   // 0xC3 : JP a16
    Cpu::op_placeholder,   // 0xC4 : CALL NZ,a16
    Cpu::op_placeholder,   // 0xC5 : PUSH BC
    Cpu::op_placeholder,   // 0xC6 : ADD A,d8
    Cpu::op_placeholder,   // 0xC7 : RST 00H
    Cpu::op_placeholder,   // 0xC8 : RET Z
    Cpu::op_placeholder,   // 0xC9 : RET
    Cpu::op_placeholder,   // 0xCA : JP Z,a16
    Cpu::op_placeholder,   // 0xCB : PREFIX CB
    Cpu::op_placeholder,   // 0xCC : CALL Z,a16
    Cpu::op_placeholder,   // 0xCD : CALL a16
    Cpu::op_placeholder,   // 0xCE : ADC A,d8
    Cpu::op_placeholder,   // 0xCF : RST 08H
    Cpu::op_placeholder,   // 0xD0 : RET NC
    Cpu::op_placeholder,   // 0xD1 : POP DE
    Cpu::op_placeholder,   // 0xD2 : JP NC,a16
    Cpu::op_placeholder,   // 0xD3 : undefined
    Cpu::op_placeholder,   // 0xD4 : CALL NC,a16
    Cpu::op_placeholder,   // 0xD5 : PUSH DE
    Cpu::op_placeholder,   // 0xD6 : SUB d8
    Cpu::op_placeholder,   // 0xD7 : RST 10H
    Cpu::op_placeholder,   // 0xD8 : RET C
    Cpu::op_placeholder,   // 0xD9 : RETI
    Cpu::op_placeholder,   // 0xDA : JP C,a16
    Cpu::op_placeholder,   // 0xDB : undefined
    Cpu::op_placeholder,   // 0xDC : CALL C,a16
    Cpu::op_placeholder,   // 0xDD : undefined
    Cpu::op_placeholder,   // 0xDE : SBC A,d8
    Cpu::op_placeholder,   // 0xDF : RST 18H
    Cpu::op_placeholder,   // 0xE0 : LDH (a8),A
    Cpu::op_placeholder,   // 0xE1 : POP HL
    Cpu::op_placeholder,   // 0xE2 : LD (C),A
    Cpu::op_placeholder,   // 0xE3 : undefined
    Cpu::op_placeholder,   // 0xE4 : undefined
    Cpu::op_placeholder,   // 0xE5 : PUSH HL
    Cpu::op_placeholder,   // 0xE6 : AND d8
    Cpu::op_placeholder,   // 0xE7 : RST 20H
    Cpu::op_placeholder,   // 0xE8 : ADD SP,r8
    Cpu::op_placeholder,   // 0xE9 : JP (HL)
    Cpu::op_placeholder,   // 0xEA : LD (a16),A
    Cpu::op_placeholder,   // 0xEB : undefined
    Cpu::op_placeholder,   // 0xEC : undefined
    Cpu::op_placeholder,   // 0xED : undefined
    Cpu::op_placeholder,   // 0xEE : XOR d8
    Cpu::op_placeholder,   // 0xEF : RST 28H
    Cpu::op_placeholder,   // 0xF0 : LDH A,(a8)
    Cpu::op_placeholder,   // 0xF1 : POP AF
    Cpu::op_placeholder,   // 0xF2 : LD A,(C)
    Cpu::op_placeholder,   // 0xF3 : DI
    Cpu::op_placeholder,   // 0xF4 : undefined
    Cpu::op_placeholder,   // 0xF5 : PUSH AF
    Cpu::op_placeholder,   // 0xF6 : OR d8
    Cpu::op_placeholder,   // 0xF7 : RST 30H
    Cpu::op_placeholder,   // 0xF8 : LD HL,SP+r8
    Cpu::op_placeholder,   // 0xF9 : LD SP,HL
    Cpu::op_placeholder,   // 0xFA : LD A,(a16)
    Cpu::op_placeholder,   // 0xFB : EI
    Cpu::op_placeholder,   // 0xFC : undefined
    Cpu::op_placeholder,   // 0xFD : undefined
    Cpu::op_placeholder,   // 0xFE : CP d8
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

    fn de(&self) -> u16 {
        ((self.register_d as u16) << 8) | self.register_e as u16
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
}

impl Cpu {
    pub fn new(memory: Box<dyn Memory>) -> Self {
        Self {
            memory,
            program_counter: 0,
            registers: CpuRegisters::new(),
            status_flags: 0,
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

macro_rules! op_load_r_u8 {
    ($x:tt) => {
        paste! {
            fn [< op_load_ $x _u8 >] (&mut self) {
                self.registers. [< register_ $x >] = self.fetch_next_byte();
            }
        }
    };
}

impl Cpu {
    op_load_r_u8!(b);
    op_load_r_u8!(c);
    op_load_r_u8!(d);
    op_load_r_u8!(e);
    op_load_r_u8!(h);
    op_load_r_u8!(l);
    op_load_r_u8!(a);
}

macro_rules! op_load_r_r {
    ($x:tt, $y:tt) => {
        paste! {
            fn [< op_load_ $x _ $y >] (&mut self) {
                self.registers. [< register_ $x >] = self.registers. [< register_ $y >];
            }
        }
    };
}

impl Cpu {
    op_load_r_r!(b, b);
    op_load_r_r!(b, c);
    op_load_r_r!(b, d);
    op_load_r_r!(b, e);
    op_load_r_r!(b, h);
    op_load_r_r!(b, l);
    op_load_r_r!(b, a);

    op_load_r_r!(c, b);
    op_load_r_r!(c, c);
    op_load_r_r!(c, d);
    op_load_r_r!(c, e);
    op_load_r_r!(c, h);
    op_load_r_r!(c, l);
    op_load_r_r!(c, a);

    op_load_r_r!(d, b);
    op_load_r_r!(d, c);
    op_load_r_r!(d, d);
    op_load_r_r!(d, e);
    op_load_r_r!(d, h);
    op_load_r_r!(d, l);
    op_load_r_r!(d, a);

    op_load_r_r!(e, b);
    op_load_r_r!(e, c);
    op_load_r_r!(e, d);
    op_load_r_r!(e, e);
    op_load_r_r!(e, h);
    op_load_r_r!(e, l);
    op_load_r_r!(e, a);

    op_load_r_r!(h, b);
    op_load_r_r!(h, c);
    op_load_r_r!(h, d);
    op_load_r_r!(h, e);
    op_load_r_r!(h, h);
    op_load_r_r!(h, l);
    op_load_r_r!(h, a);

    op_load_r_r!(l, b);
    op_load_r_r!(l, c);
    op_load_r_r!(l, d);
    op_load_r_r!(l, e);
    op_load_r_r!(l, h);
    op_load_r_r!(l, l);
    op_load_r_r!(l, a);

    op_load_r_r!(a, b);
    op_load_r_r!(a, c);
    op_load_r_r!(a, d);
    op_load_r_r!(a, e);
    op_load_r_r!(a, h);
    op_load_r_r!(a, l);
    op_load_r_r!(a, a);
}

macro_rules! op_load_hl_r {
    ($x:tt) => {
        paste! {
            fn [< op_load_hl _ $x >] (&mut self) {
                self.memory
                    .write(self.registers.hl(), self.registers.[< register_ $x >]);
            }
        }
    };
}

impl Cpu {
    op_load_hl_r!(b);
    op_load_hl_r!(c);
    op_load_hl_r!(d);
    op_load_hl_r!(e);
    op_load_hl_r!(h);
    op_load_hl_r!(l);
    op_load_hl_r!(a);
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

macro_rules! op_add_a_r {
    ($x:tt) => {
        paste! {
            fn [< op_add_a_ $x >] (&mut self) {
                let result: u16 = (self.registers.register_a as u16) + (self.registers.[< register_ $x >] as u16);
                self.status_flags = 0;

                if (result & 0xFF) == 0 {
                    self.status_flags |= STATUS_FLAG_Z;
                }

                if ((self.registers.register_a & 0xF) + (self.registers.[< register_ $x >] & 0xF)) > 0xF {
                    self.status_flags |= STATUS_FLAG_H;
                }

                if result > 0xFF {
                    self.status_flags |= STATUS_FLAG_C;
                }

                self.registers.register_a = (result & 0xFF) as u8;
            }
        }
    };
}

impl Cpu {
    op_add_a_r!(b);
    op_add_a_r!(c);
    op_add_a_r!(d);
    op_add_a_r!(e);
    op_add_a_r!(h);
    op_add_a_r!(l);
    op_add_a_r!(a);

    fn op_add_a_hl(&mut self) {
        let operand = self.memory.read(self.registers.hl());
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
}

macro_rules! op_adc_a_r {
    ($x:tt) => {
        paste! {
            fn [< op_adc_a_ $x >] (&mut self) {
                let carry: u16 = if (self.status_flags & STATUS_FLAG_C) != 0 {
                    1
                } else {
                    0
                };

                let result: u16 = (self.registers.register_a as u16) + (self.registers.[< register_ $x >] as u16) + carry;
                self.status_flags = 0;

                if (result & 0xFF) == 0 {
                    self.status_flags |= STATUS_FLAG_Z;
                }

                if ((self.registers.register_a & 0xF) + (self.registers.[< register_ $x >] & 0xF) + (carry as u8)) > 0xF {
                    self.status_flags |= STATUS_FLAG_H;
                }

                if result > 0xFF {
                    self.status_flags |= STATUS_FLAG_C;
                }

                self.registers.register_a = (result & 0xFF) as u8;
            }
        }
    };
}

impl Cpu {
    op_adc_a_r!(b);
    op_adc_a_r!(c);
    op_adc_a_r!(d);
    op_adc_a_r!(e);
    op_adc_a_r!(h);
    op_adc_a_r!(l);
    op_adc_a_r!(a);

    fn op_adc_a_hl(&mut self) {
        let carry: u16 = if (self.status_flags & STATUS_FLAG_C) != 0 {
            1
        } else {
            0
        };

        let operand = self.memory.read(self.registers.hl());
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
}

macro_rules! op_sub_a_r {
    ($x:tt) => {
        paste! {
            fn [< op_sub_a_ $x >] (&mut self) {
                self.status_flags = STATUS_FLAG_N;

                if self.registers.register_a == self.registers.[< register_ $x >] {
                    self.status_flags |= STATUS_FLAG_Z;
                }

                if (self.registers.register_a & 0xF) < (self.registers.[< register_ $x >] & 0xF) {
                    self.status_flags |= STATUS_FLAG_H;
                }

                if self.registers.register_a < self.registers.[< register_ $x >] {
                    self.status_flags |= STATUS_FLAG_C;
                }

                self.registers.register_a = self.registers.register_a.wrapping_sub(self.registers.[< register_ $x >]);
            }
        }
    };
}

impl Cpu {
    op_sub_a_r!(b);
    op_sub_a_r!(c);
    op_sub_a_r!(d);
    op_sub_a_r!(e);
    op_sub_a_r!(h);
    op_sub_a_r!(l);
    op_sub_a_r!(a);

    fn op_sub_a_hl(&mut self) {
        let operand = self.memory.read(self.registers.hl());
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
}

macro_rules! op_sbc_a_r {
    ($x:tt) => {
        paste! {
            fn [< op_sbc_a_ $x >] (&mut self) {
                let carry: u8 = if (self.status_flags & STATUS_FLAG_C) != 0 {
                    1
                } else {
                    0
                };

                let result = self.registers.register_a.wrapping_sub(self.registers.[< register_ $x >]).wrapping_sub(carry);
                self.status_flags = STATUS_FLAG_N;

                if result == 0 {
                    self.status_flags |= STATUS_FLAG_Z;
                }

                if (self.registers.register_a & 0xF) < ((self.registers.[< register_ $x >] & 0xF) + carry) {
                    self.status_flags |= STATUS_FLAG_H;
                }

                if (self.registers.register_a as u16)
                    .wrapping_sub(self.registers.[< register_ $x >] as u16)
                    .wrapping_sub(carry as u16)
                    > 0xFF
                {
                    self.status_flags |= STATUS_FLAG_C;
                }

                self.registers.register_a = result;
            }
        }
    };
}

impl Cpu {
    op_sbc_a_r!(b);
    op_sbc_a_r!(c);
    op_sbc_a_r!(d);
    op_sbc_a_r!(e);
    op_sbc_a_r!(h);
    op_sbc_a_r!(l);
    op_sbc_a_r!(a);

    fn op_sbc_a_hl(&mut self) {
        let carry: u8 = if (self.status_flags & STATUS_FLAG_C) != 0 {
            1
        } else {
            0
        };

        let operand = self.memory.read(self.registers.hl());
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
}

macro_rules! op_and_a_r {
    ($x:tt) => {
        paste! {
            fn [< op_and_a_ $x >] (&mut self) {
                self.registers.register_a &= self.registers.[< register_ $x >];
                self.status_flags = STATUS_FLAG_H;

                if self.registers.register_a == 0 {
                    self.status_flags |= STATUS_FLAG_Z;
                }
            }
        }
    };
}

impl Cpu {
    op_and_a_r!(b);
    op_and_a_r!(c);
    op_and_a_r!(d);
    op_and_a_r!(e);
    op_and_a_r!(h);
    op_and_a_r!(l);
    op_and_a_r!(a);

    fn op_and_a_hl(&mut self) {
        self.registers.register_a &= self.memory.read(self.registers.hl());
        self.status_flags = STATUS_FLAG_H;

        if self.registers.register_a == 0 {
            self.status_flags |= STATUS_FLAG_Z;
        }
    }
}

macro_rules! op_xor_a_r {
    ($x:tt) => {
        paste! {
            fn [< op_xor_a_ $x >] (&mut self) {
                self.registers.register_a ^= self.registers.[< register_ $x >];
                self.status_flags = STATUS_FLAG_H;

                if self.registers.register_a == 0 {
                    self.status_flags |= STATUS_FLAG_Z;
                }
            }
        }
    };
}

impl Cpu {
    op_xor_a_r!(b);
    op_xor_a_r!(c);
    op_xor_a_r!(d);
    op_xor_a_r!(e);
    op_xor_a_r!(h);
    op_xor_a_r!(l);
    op_xor_a_r!(a);

    fn op_xor_a_hl(&mut self) {
        self.registers.register_a ^= self.memory.read(self.registers.hl());
        self.status_flags = STATUS_FLAG_H;

        if self.registers.register_a == 0 {
            self.status_flags |= STATUS_FLAG_Z;
        }
    }
}

macro_rules! op_or_a_r {
    ($x:tt) => {
        paste! {
            fn [< op_or_a_ $x >] (&mut self) {
                self.registers.register_a |= self.registers.[< register_ $x >];
                self.status_flags = STATUS_FLAG_H;

                if self.registers.register_a == 0 {
                    self.status_flags |= STATUS_FLAG_Z;
                }
            }
        }
    };
}

impl Cpu {
    op_or_a_r!(b);
    op_or_a_r!(c);
    op_or_a_r!(d);
    op_or_a_r!(e);
    op_or_a_r!(h);
    op_or_a_r!(l);
    op_or_a_r!(a);

    fn op_or_a_hl(&mut self) {
        self.registers.register_a |= self.memory.read(self.registers.hl());
        self.status_flags = STATUS_FLAG_H;

        if self.registers.register_a == 0 {
            self.status_flags |= STATUS_FLAG_Z;
        }
    }
}
