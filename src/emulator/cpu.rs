use super::Console;

use std::rc::Rc;

const OP_CODE_FUNCTION_TABLE: [fn(&mut Cpu); 256] = [
    Cpu::op_nop,         // 0x00 : NOP
    Cpu::op_placeholder, // 0x01 : LD BC,d16
    Cpu::op_placeholder, // 0x02 : LD (BC),A
    Cpu::op_placeholder, // 0x03 : INC BC
    Cpu::op_placeholder, // 0x04 : INC B
    Cpu::op_placeholder, // 0x05 : DEC B
    Cpu::op_placeholder, // 0x06 : LD B,d8
    Cpu::op_placeholder, // 0x07 : RLCA
    Cpu::op_placeholder, // 0x08 : LD (a16),SP
    Cpu::op_placeholder, // 0x09 : ADD HL,BC
    Cpu::op_placeholder, // 0x0A : LD A,(BC)
    Cpu::op_placeholder, // 0x0B : DEC BC
    Cpu::op_placeholder, // 0x0C : INC C
    Cpu::op_placeholder, // 0x0D : DEC C
    Cpu::op_placeholder, // 0x0E : LD C,d8
    Cpu::op_placeholder, // 0x0F : RRCA
    Cpu::op_placeholder, // 0x10 : STOP 0
    Cpu::op_placeholder, // 0x11 : LD DE,d16
    Cpu::op_placeholder, // 0x12 : LD (DE),A
    Cpu::op_placeholder, // 0x13 : INC DE
    Cpu::op_placeholder, // 0x14 : INC D
    Cpu::op_placeholder, // 0x15 : DEC D
    Cpu::op_placeholder, // 0x16 : LD D,d8
    Cpu::op_placeholder, // 0x17 : RLA
    Cpu::op_placeholder, // 0x18 : JR r8
    Cpu::op_placeholder, // 0x19 : ADD HL,DE
    Cpu::op_placeholder, // 0x1A : LD A,(DE)
    Cpu::op_placeholder, // 0x1B : DEC DE
    Cpu::op_placeholder, // 0x1C : INC E
    Cpu::op_placeholder, // 0x1D : DEC E
    Cpu::op_placeholder, // 0x1E : LD E,d8
    Cpu::op_placeholder, // 0x1F : RRA
    Cpu::op_placeholder, // 0x20 : JR NZ,r8
    Cpu::op_placeholder, // 0x21 : LD HL,d16
    Cpu::op_placeholder, // 0x22 : LD (HL+),A
    Cpu::op_placeholder, // 0x23 : INC HL
    Cpu::op_placeholder, // 0x24 : INC H
    Cpu::op_placeholder, // 0x25 : DEC H
    Cpu::op_placeholder, // 0x26 : LD H,d8
    Cpu::op_placeholder, // 0x27 : DAA
    Cpu::op_placeholder, // 0x28 : JR Z,r8
    Cpu::op_placeholder, // 0x29 : ADD HL,HL
    Cpu::op_placeholder, // 0x2A : LD A,(HL+)
    Cpu::op_placeholder, // 0x2B : DEC HL
    Cpu::op_placeholder, // 0x2C : INC L
    Cpu::op_placeholder, // 0x2D : DEC L
    Cpu::op_placeholder, // 0x2E : LD L,d8
    Cpu::op_placeholder, // 0x2F : CPL
    Cpu::op_placeholder, // 0x30 : JR NC,r8
    Cpu::op_placeholder, // 0x31 : LD SP,d16
    Cpu::op_placeholder, // 0x32 : LD (HL-),A
    Cpu::op_placeholder, // 0x33 : INC SP
    Cpu::op_placeholder, // 0x34 : INC (HL)
    Cpu::op_placeholder, // 0x35 : DEC (HL)
    Cpu::op_placeholder, // 0x36 : LD (HL),d8
    Cpu::op_placeholder, // 0x37 : SCF
    Cpu::op_placeholder, // 0x38 : JR C,r8
    Cpu::op_placeholder, // 0x39 : ADD HL,SP
    Cpu::op_placeholder, // 0x3A : LD A,(HL-)
    Cpu::op_placeholder, // 0x3B : DEC SP
    Cpu::op_placeholder, // 0x3C : INC A
    Cpu::op_placeholder, // 0x3D : DEC A
    Cpu::op_placeholder, // 0x3E : LD A,d8
    Cpu::op_placeholder, // 0x3F : CCF
    Cpu::op_placeholder, // 0x40 : LD B,B
    Cpu::op_placeholder, // 0x41 : LD B,C
    Cpu::op_placeholder, // 0x42 : LD B,D
    Cpu::op_placeholder, // 0x43 : LD B,E
    Cpu::op_placeholder, // 0x44 : LD B,H
    Cpu::op_placeholder, // 0x45 : LD B,L
    Cpu::op_placeholder, // 0x46 : LD B,(HL)
    Cpu::op_placeholder, // 0x47 : LD B,A
    Cpu::op_placeholder, // 0x48 : LD C,B
    Cpu::op_placeholder, // 0x49 : LD C,C
    Cpu::op_placeholder, // 0x4A : LD C,D
    Cpu::op_placeholder, // 0x4B : LD C,E
    Cpu::op_placeholder, // 0x4C : LD C,H
    Cpu::op_placeholder, // 0x4D : LD C,L
    Cpu::op_placeholder, // 0x4E : LD C,(HL)
    Cpu::op_placeholder, // 0x4F : LD C,A
    Cpu::op_placeholder, // 0x50 : LD D,B
    Cpu::op_placeholder, // 0x51 : LD D,C
    Cpu::op_placeholder, // 0x52 : LD D,D
    Cpu::op_placeholder, // 0x53 : LD D,E
    Cpu::op_placeholder, // 0x54 : LD D,H
    Cpu::op_placeholder, // 0x55 : LD D,L
    Cpu::op_placeholder, // 0x56 : LD D,(HL)
    Cpu::op_placeholder, // 0x57 : LD D,A
    Cpu::op_placeholder, // 0x58 : LD E,B
    Cpu::op_placeholder, // 0x59 : LD E,C
    Cpu::op_placeholder, // 0x5A : LD E,D
    Cpu::op_placeholder, // 0x5B : LD E,E
    Cpu::op_placeholder, // 0x5C : LD E,H
    Cpu::op_placeholder, // 0x5D : LD E,L
    Cpu::op_placeholder, // 0x5E : LD E,(HL)
    Cpu::op_placeholder, // 0x5F : LD E,A
    Cpu::op_placeholder, // 0x60 : LD H,B
    Cpu::op_placeholder, // 0x61 : LD H,C
    Cpu::op_placeholder, // 0x62 : LD H,D
    Cpu::op_placeholder, // 0x63 : LD H,E
    Cpu::op_placeholder, // 0x64 : LD H,H
    Cpu::op_placeholder, // 0x65 : LD H,L
    Cpu::op_placeholder, // 0x66 : LD H,(HL)
    Cpu::op_placeholder, // 0x67 : LD H,A
    Cpu::op_placeholder, // 0x68 : LD L,B
    Cpu::op_placeholder, // 0x69 : LD L,C
    Cpu::op_placeholder, // 0x6A : LD L,D
    Cpu::op_placeholder, // 0x6B : LD L,E
    Cpu::op_placeholder, // 0x6C : LD L,H
    Cpu::op_placeholder, // 0x6D : LD L,L
    Cpu::op_placeholder, // 0x6E : LD L,(HL)
    Cpu::op_placeholder, // 0x6F : LD L,A
    Cpu::op_placeholder, // 0x70 : LD (HL),B
    Cpu::op_placeholder, // 0x71 : LD (HL),C
    Cpu::op_placeholder, // 0x72 : LD (HL),D
    Cpu::op_placeholder, // 0x73 : LD (HL),E
    Cpu::op_placeholder, // 0x74 : LD (HL),H
    Cpu::op_placeholder, // 0x75 : LD (HL),L
    Cpu::op_placeholder, // 0x76 : HALT
    Cpu::op_placeholder, // 0x77 : LD (HL),A
    Cpu::op_placeholder, // 0x78 : LD A,B
    Cpu::op_placeholder, // 0x79 : LD A,C
    Cpu::op_placeholder, // 0x7A : LD A,D
    Cpu::op_placeholder, // 0x7B : LD A,E
    Cpu::op_placeholder, // 0x7C : LD A,H
    Cpu::op_placeholder, // 0x7D : LD A,L
    Cpu::op_placeholder, // 0x7E : LD A,(HL)
    Cpu::op_placeholder, // 0x7F : LD A,A
    Cpu::op_placeholder, // 0x80 : ADD A,B
    Cpu::op_placeholder, // 0x81 : ADD A,C
    Cpu::op_placeholder, // 0x82 : ADD A,D
    Cpu::op_placeholder, // 0x83 : ADD A,E
    Cpu::op_placeholder, // 0x84 : ADD A,H
    Cpu::op_placeholder, // 0x85 : ADD A,L
    Cpu::op_placeholder, // 0x86 : ADD A,(HL)
    Cpu::op_placeholder, // 0x87 : ADD A,A
    Cpu::op_placeholder, // 0x88 : ADC A,B
    Cpu::op_placeholder, // 0x89 : ADC A,C
    Cpu::op_placeholder, // 0x8A : ADC A,D
    Cpu::op_placeholder, // 0x8B : ADC A,E
    Cpu::op_placeholder, // 0x8C : ADC A,H
    Cpu::op_placeholder, // 0x8D : ADC A,L
    Cpu::op_placeholder, // 0x8E : ADC A,(HL)
    Cpu::op_placeholder, // 0x8F : ADC A,A
    Cpu::op_placeholder, // 0x90 : SUB B
    Cpu::op_placeholder, // 0x91 : SUB C
    Cpu::op_placeholder, // 0x92 : SUB D
    Cpu::op_placeholder, // 0x93 : SUB E
    Cpu::op_placeholder, // 0x94 : SUB H
    Cpu::op_placeholder, // 0x95 : SUB L
    Cpu::op_placeholder, // 0x96 : SUB (HL)
    Cpu::op_placeholder, // 0x97 : SUB A
    Cpu::op_placeholder, // 0x98 : SBC A,B
    Cpu::op_placeholder, // 0x99 : SBC A,C
    Cpu::op_placeholder, // 0x9A : SBC A,D
    Cpu::op_placeholder, // 0x9B : SBC A,E
    Cpu::op_placeholder, // 0x9C : SBC A,H
    Cpu::op_placeholder, // 0x9D : SBC A,L
    Cpu::op_placeholder, // 0x9E : SBC A,(HL)
    Cpu::op_placeholder, // 0x9F : SBC A,A
    Cpu::op_placeholder, // 0xA0 : AND B
    Cpu::op_placeholder, // 0xA1 : AND C
    Cpu::op_placeholder, // 0xA2 : AND D
    Cpu::op_placeholder, // 0xA3 : AND E
    Cpu::op_placeholder, // 0xA4 : AND H
    Cpu::op_placeholder, // 0xA5 : AND L
    Cpu::op_placeholder, // 0xA6 : AND (HL)
    Cpu::op_placeholder, // 0xA7 : AND A
    Cpu::op_placeholder, // 0xA8 : XOR B
    Cpu::op_placeholder, // 0xA9 : XOR C
    Cpu::op_placeholder, // 0xAA : XOR D
    Cpu::op_placeholder, // 0xAB : XOR E
    Cpu::op_placeholder, // 0xAC : XOR H
    Cpu::op_placeholder, // 0xAD : XOR L
    Cpu::op_placeholder, // 0xAE : XOR (HL)
    Cpu::op_placeholder, // 0xAF : XOR A
    Cpu::op_placeholder, // 0xB0 : OR B
    Cpu::op_placeholder, // 0xB1 : OR C
    Cpu::op_placeholder, // 0xB2 : OR D
    Cpu::op_placeholder, // 0xB3 : OR E
    Cpu::op_placeholder, // 0xB4 : OR H
    Cpu::op_placeholder, // 0xB5 : OR L
    Cpu::op_placeholder, // 0xB6 : OR (HL)
    Cpu::op_placeholder, // 0xB7 : OR A
    Cpu::op_placeholder, // 0xB8 : CP B
    Cpu::op_placeholder, // 0xB9 : CP C
    Cpu::op_placeholder, // 0xBA : CP D
    Cpu::op_placeholder, // 0xBB : CP E
    Cpu::op_placeholder, // 0xBC : CP H
    Cpu::op_placeholder, // 0xBD : CP L
    Cpu::op_placeholder, // 0xBE : CP (HL)
    Cpu::op_placeholder, // 0xBF : CP A
    Cpu::op_placeholder, // 0xC0 : RET NZ
    Cpu::op_placeholder, // 0xC1 : POP BC
    Cpu::op_placeholder, // 0xC2 : JP NZ,a16
    Cpu::op_placeholder, // 0xC3 : JP a16
    Cpu::op_placeholder, // 0xC4 : CALL NZ,a16
    Cpu::op_placeholder, // 0xC5 : PUSH BC
    Cpu::op_placeholder, // 0xC6 : ADD A,d8
    Cpu::op_placeholder, // 0xC7 : RST 00H
    Cpu::op_placeholder, // 0xC8 : RET Z
    Cpu::op_placeholder, // 0xC9 : RET
    Cpu::op_placeholder, // 0xCA : JP Z,a16
    Cpu::op_placeholder, // 0xCB : PREFIX CB
    Cpu::op_placeholder, // 0xCC : CALL Z,a16
    Cpu::op_placeholder, // 0xCD : CALL a16
    Cpu::op_placeholder, // 0xCE : ADC A,d8
    Cpu::op_placeholder, // 0xCF : RST 08H
    Cpu::op_placeholder, // 0xD0 : RET NC
    Cpu::op_placeholder, // 0xD1 : POP DE
    Cpu::op_placeholder, // 0xD2 : JP NC,a16
    Cpu::op_placeholder, // 0xD3 : undefined
    Cpu::op_placeholder, // 0xD4 : CALL NC,a16
    Cpu::op_placeholder, // 0xD5 : PUSH DE
    Cpu::op_placeholder, // 0xD6 : SUB d8
    Cpu::op_placeholder, // 0xD7 : RST 10H
    Cpu::op_placeholder, // 0xD8 : RET C
    Cpu::op_placeholder, // 0xD9 : RETI
    Cpu::op_placeholder, // 0xDA : JP C,a16
    Cpu::op_placeholder, // 0xDB : undefined
    Cpu::op_placeholder, // 0xDC : CALL C,a16
    Cpu::op_placeholder, // 0xDD : undefined
    Cpu::op_placeholder, // 0xDE : SBC A,d8
    Cpu::op_placeholder, // 0xDF : RST 18H
    Cpu::op_placeholder, // 0xE0 : LDH (a8),A
    Cpu::op_placeholder, // 0xE1 : POP HL
    Cpu::op_placeholder, // 0xE2 : LD (C),A
    Cpu::op_placeholder, // 0xE3 : undefined
    Cpu::op_placeholder, // 0xE4 : undefined
    Cpu::op_placeholder, // 0xE5 : PUSH HL
    Cpu::op_placeholder, // 0xE6 : AND d8
    Cpu::op_placeholder, // 0xE7 : RST 20H
    Cpu::op_placeholder, // 0xE8 : ADD SP,r8
    Cpu::op_placeholder, // 0xE9 : JP (HL)
    Cpu::op_placeholder, // 0xEA : LD (a16),A
    Cpu::op_placeholder, // 0xEB : undefined
    Cpu::op_placeholder, // 0xEC : undefined
    Cpu::op_placeholder, // 0xED : undefined
    Cpu::op_placeholder, // 0xEE : XOR d8
    Cpu::op_placeholder, // 0xEF : RST 28H
    Cpu::op_placeholder, // 0xF0 : LDH A,(a8)
    Cpu::op_placeholder, // 0xF1 : POP AF
    Cpu::op_placeholder, // 0xF2 : LD A,(C)
    Cpu::op_placeholder, // 0xF3 : DI
    Cpu::op_placeholder, // 0xF4 : undefined
    Cpu::op_placeholder, // 0xF5 : PUSH AF
    Cpu::op_placeholder, // 0xF6 : OR d8
    Cpu::op_placeholder, // 0xF7 : RST 30H
    Cpu::op_placeholder, // 0xF8 : LD HL,SP+r8
    Cpu::op_placeholder, // 0xF9 : LD SP,HL
    Cpu::op_placeholder, // 0xFA : LD A,(a16)
    Cpu::op_placeholder, // 0xFB : EI
    Cpu::op_placeholder, // 0xFC : undefined
    Cpu::op_placeholder, // 0xFD : undefined
    Cpu::op_placeholder, // 0xFE : CP d8
    Cpu::op_placeholder, // 0xFF : RST 38H
];

pub struct Cpu {
    console: Rc<Console>,
    program_counter: u16,
}

impl Cpu {
    pub fn new(console: Rc<Console>) -> Self {
        Self {
            console,
            program_counter: 0,
        }
    }

    pub fn tick(&mut self) {
        OP_CODE_FUNCTION_TABLE[self.fetch_next_byte() as usize](self);
    }

    fn op_nop(&mut self) {}

    fn op_placeholder(&mut self) {
        panic!("Opcode not implemented!")
    }

    fn fetch_next_byte(&mut self) -> u8 {
        let value = self.console.read(self.program_counter);
        self.program_counter += 1;

        value
    }
}
