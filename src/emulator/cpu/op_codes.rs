use super::cb_codes::CB_CODE_FUNCTION_TABLE;
use super::{status_flag, Cpu};

/// Gameboy SM63 opcode implementations
impl Cpu {
    /// Opcode 0x00: [NOP](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=34)
    ///
    /// No operation. This instruction doesn't do anything, but can be used to add a
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
        self.write(self.registers.bc(), self.registers.a());
    }

    /// Opcode 0x03: [INC BC](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=72)
    ///
    /// Increments data in the 16-bit register BC (2 machine cycles).
    fn op_inc_bc(&mut self) {
        self.dummy_cycle();
        self.registers.set_bc(self.registers.bc().wrapping_add(1));
    }

    /// Opcode 0x04: [INC B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=55)
    ///
    /// Increments data in the 8-bit register B (1 machine cycle).
    fn op_inc_b(&mut self) {
        let value = self.run_inc_u8_and_update_flags(self.registers.b());
        self.registers.set_b(value);
    }

    /// Opcode 0x05: [DEC B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=57)
    ///
    /// Decrements data in the 8-bit register B (1 machine cycle).
    fn op_dec_b(&mut self) {
        let value = self.run_dec_u8_and_update_flags(self.registers.b());
        self.registers.set_b(value);
    }

    /// Opcode 0x06: [LD B,d8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=16)
    ///
    /// Load to the 8-bit register B, the immediate data following the opcode (2
    /// machine cycles).
    fn op_ld_b_d8(&mut self) {
        let value = self.fetch_u8();
        self.registers.set_b(value);
    }

    /// Opcode 0x07: [RLCA](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=77)
    ///
    /// Rotate the 8-bit register B to the left (1 machine cycle).
    fn op_rlca(&mut self) {
        let carry = (self.registers.a() & 0x80) != 0;
        self.registers.set_a(self.registers.a().wrapping_shl(1));
        self.registers.reset_status_flags();

        if carry {
            self.registers.update_status_flags(status_flag::CARRY);
            self.registers.set_a(self.registers.a() | 0x01);
        }
    }

    /// Opcode 0x08: [LD (a16),SP](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=35)
    ///
    /// Load to the absolute address specified by the 16-bit operand following the
    /// opcode, data from the 16-bit SP register (5 machine cycles).
    fn op_ld_u16_sp(&mut self) {
        let address = self.fetch_u16();

        self.write(address, (self.stack_pointer & 0xFF) as u8);
        self.write(address.wrapping_add(1), (self.stack_pointer >> 8) as u8);
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
        let value = self.read(self.registers.bc());
        self.registers.set_a(value);
    }

    /// Opcode 0x0B: [DEC BC](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=73)
    ///
    /// DEcrements data in the 16-bit register BC (2 machine cycles).
    fn op_dec_bc(&mut self) {
        self.dummy_cycle();
        self.registers.set_bc(self.registers.bc().wrapping_sub(1));
    }

    /// Opcode 0x0C: [INC C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=55)
    ///
    /// Increments data in the 8-bit register C (1 machine cycle).
    fn op_inc_c(&mut self) {
        let value = self.run_inc_u8_and_update_flags(self.registers.c());
        self.registers.set_c(value);
    }

    /// Opcode 0x0D: [DEC C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=57)
    ///
    /// Decrements data in the 8-bit register C (1 machine cycle).
    fn op_dec_c(&mut self) {
        let value = self.run_dec_u8_and_update_flags(self.registers.c());
        self.registers.set_c(value);
    }

    /// Opcode 0x0E: [LD C,d8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=16)
    ///
    /// Load to the 8-bit register C, the immediate data following the opcode (2
    /// machine cycles).
    fn op_ld_c_d8(&mut self) {
        let value = self.fetch_u8();
        self.registers.set_c(value);
    }

    /// Opcode 0x0F: [RRCA](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=78)
    ///
    /// Rotate the 8-bit register B to the right (1 machine cycle).
    fn op_rrca(&mut self) {
        let carry = (self.registers.a() & 0x01) != 0;
        self.registers.set_a(self.registers.a().wrapping_shr(1));
        self.registers.reset_status_flags();

        if carry {
            self.registers.update_status_flags(status_flag::CARRY);
            self.registers.set_a(self.registers.a() | 0x80);
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
        self.write(self.registers.de(), self.registers.a());
    }

    /// Opcode 0x13: [INC DE](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=72)
    ///
    /// Increments data in the 16-bit register DE (2 machine cycles).
    fn op_inc_de(&mut self) {
        self.dummy_cycle();
        self.registers.set_de(self.registers.de().wrapping_add(1));
    }

    /// Opcode 0x14: [INC D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=55)
    ///
    /// Increments data in the 8-bit register D (1 machine cycle).
    fn op_inc_d(&mut self) {
        let value = self.run_inc_u8_and_update_flags(self.registers.d());
        self.registers.set_d(value);
    }

    /// Opcode 0x15: [DEC D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=57)
    ///
    /// Decrements data in the 8-bit register D (1 machine cycle).
    fn op_dec_d(&mut self) {
        let value = self.run_dec_u8_and_update_flags(self.registers.d());
        self.registers.set_d(value);
    }

    /// Opcode 0x16: [LD D,d8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=16)
    ///
    /// Load to the 8-bit register D, the immediate data following the opcode (2
    /// machine cycles).
    fn op_ld_d_d8(&mut self) {
        let value = self.fetch_u8();
        self.registers.set_d(value);
    }

    /// Opcode 0x17: [RLA](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=79)
    ///
    /// Rotate the 8-bit register B to the left (1 machine cycle).
    fn op_rla(&mut self) {
        let carry = (self.registers.a() & 0x80) != 0;
        self.registers.set_a(self.registers.a().wrapping_shl(1));

        if (self.registers.status_flags() & status_flag::CARRY) != 0 {
            self.registers.set_a(self.registers.a() | 0x01);
        }

        self.registers.reset_status_flags();

        if carry {
            self.registers.update_status_flags(status_flag::CARRY);
        }
    }

    /// Opcode 0x17: [JR r8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=108)
    ///
    /// Unconditional jump to the relative address specified by the signed 8-bit
    /// operand following the opcode (3 machine cycles).
    fn op_jr_r8(&mut self) {
        let offset = self.fetch_u8() as i8;
        self.dummy_cycle();
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
        let value = self.read(self.registers.de());
        self.registers.set_a(value);
    }

    /// Opcode 0x1B: [DEC DE](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=73)
    ///
    /// DEcrements data in the 16-bit register DE (2 machine cycles).
    fn op_dec_de(&mut self) {
        self.dummy_cycle();
        self.registers.set_de(self.registers.de().wrapping_sub(1));
    }

    /// Opcode 0x1C: [INC E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=55)
    ///
    /// Increments data in the 8-bit register E (1 machine cycle).
    fn op_inc_e(&mut self) {
        let value = self.run_inc_u8_and_update_flags(self.registers.e());
        self.registers.set_e(value);
    }

    /// Opcode 0x1D: [DEC E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=57)
    ///
    /// Decrements data in the 8-bit register E (1 machine cycle).
    fn op_dec_e(&mut self) {
        let value = self.run_dec_u8_and_update_flags(self.registers.e());
        self.registers.set_e(value);
    }

    /// Opcode 0x1E: [LD E,d8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=16)
    ///
    /// Load to the 8-bit register E, the immediate data following the opcode (2
    /// machine cycles).
    fn op_ld_e_d8(&mut self) {
        let value = self.fetch_u8();
        self.registers.set_e(value);
    }

    /// Opcode 0x1F: [RRA](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=80)
    ///
    /// Rotate the 8-bit register B to the right (1 machine cycle).
    fn op_rra(&mut self) {
        let carry = (self.registers.a() & 0x01) != 0;
        self.registers.set_a(self.registers.a().wrapping_shr(1));

        if (self.registers.status_flags() & status_flag::CARRY) != 0 {
            self.registers.set_a(self.registers.a() | 0x80);
        }

        self.registers.reset_status_flags();

        if carry {
            self.registers.update_status_flags(status_flag::CARRY);
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

        if (self.registers.status_flags() & status_flag::ZERO) == 0 {
            self.dummy_cycle();
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
        self.write(address, self.registers.a());
        self.registers.set_hl(address.wrapping_add(1));
    }

    /// Opcode 0x23: [INC HL](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=72)
    ///
    /// Increments data in the 16-bit register HL (2 machine cycles).
    fn op_inc_hl(&mut self) {
        self.dummy_cycle();
        self.registers.set_hl(self.registers.hl().wrapping_add(1));
    }

    /// Opcode 0x24: [INC H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=55)
    ///
    /// Increments data in the 8-bit register H (1 machine cycle).
    fn op_inc_h(&mut self) {
        let value = self.run_inc_u8_and_update_flags(self.registers.h());
        self.registers.set_h(value);
    }

    /// Opcode 0x25: [DEC H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=57)
    ///
    /// Decrements data in the 8-bit register H (1 machine cycle).
    fn op_dec_h(&mut self) {
        let value = self.run_dec_u8_and_update_flags(self.registers.h());
        self.registers.set_h(value);
    }

    /// Opcode 0x26: [LD H,d8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=16)
    ///
    /// Load to the 8-bit register H, the immediate data following the opcode (2
    /// machine cycles).
    fn op_ld_h_d8(&mut self) {
        let value = self.fetch_u8();
        self.registers.set_h(value);
    }

    /// Opcode 0x27: [DAA](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=70)
    ///
    /// Adjusts the contents of the accumulator (A register) to hold the correct packed
    /// BCD result after an arithmetic operation on packed BCD numbers (1 machine
    /// cycle).
    fn op_daa(&mut self) {
        let mut current_value = self.registers.a() as u16;

        if (self.registers.status_flags() & status_flag::NEGATIVE) == 0 {
            if (self.registers.status_flags() & status_flag::HALF_CARRY) != 0
                || (current_value & 0x0F) > 0x09
            {
                current_value = current_value.wrapping_add(0x06);
            }

            if (self.registers.status_flags() & status_flag::CARRY) != 0 || current_value > 0x9F {
                current_value = current_value.wrapping_add(0x60);
            }
        } else {
            if (self.registers.status_flags() & status_flag::HALF_CARRY) != 0 {
                current_value = current_value.wrapping_sub(0x06) & 0xFF;
            }

            if (self.registers.status_flags() & status_flag::CARRY) != 0 {
                current_value = current_value.wrapping_sub(0x60);
            }
        }

        self.registers
            .remove_status_flags(status_flag::ZERO | status_flag::HALF_CARRY);

        if (current_value & 0xFF) == 0 {
            self.registers.update_status_flags(status_flag::ZERO);
        }

        if (current_value & 0x0100) == 0x100 {
            self.registers.update_status_flags(status_flag::CARRY);
        }

        self.registers.set_a((current_value & 0xFF) as u8);
    }

    /// Opcode 0x28: [JR Z,r8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=109)
    ///
    /// Conditional jump to the relative address specified by the signed 8-bit operand
    /// following the opcode, depending on the condition Z. Note that the operand
    /// (relative address offset) is read even when the condition is false (2/3 machine
    /// cycles).
    fn op_jp_z_r8(&mut self) {
        let offset = self.fetch_u8() as i8;

        if (self.registers.status_flags() & status_flag::ZERO) != 0 {
            self.dummy_cycle();
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
        let value = self.read(address);
        self.registers.set_a(value);
        self.registers.set_hl(address.wrapping_add(1));
    }

    /// Opcode 0x2B: [DEC HL](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=73)
    ///
    /// DEcrements data in the 16-bit register HL (2 machine cycles).
    fn op_dec_hl(&mut self) {
        self.dummy_cycle();
        self.registers.set_hl(self.registers.hl().wrapping_sub(1));
    }

    /// Opcode 0x2C: [INC L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=55)
    ///
    /// Increments data in the 8-bit register L (1 machine cycle).
    fn op_inc_l(&mut self) {
        let value = self.run_inc_u8_and_update_flags(self.registers.l());
        self.registers.set_l(value);
    }

    /// Opcode 0x2D: [DEC L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=57)
    ///
    /// Decrements data in the 8-bit register L (1 machine cycle).
    fn op_dec_l(&mut self) {
        let value = self.run_dec_u8_and_update_flags(self.registers.l());
        self.registers.set_l(value);
    }

    /// Opcode 0x2E: [LD L,d8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=16)
    ///
    /// Load to the 8-bit register L, the immediate data following the opcode (2
    /// machine cycles).
    fn op_ld_l_d8(&mut self) {
        let value = self.fetch_u8();
        self.registers.set_l(value);
    }

    /// Opcode 0x2F: [CPL](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=71)
    ///
    /// Flips all the bits in the 8-bit A register, and sets the N and H flags (1
    /// machine cycle).
    fn op_cpl(&mut self) {
        self.registers.set_a(!self.registers.a());
        self.registers
            .update_status_flags(status_flag::HALF_CARRY | status_flag::NEGATIVE);
    }

    /// Opcode 0x30: [JR NC,r8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=109)
    ///
    /// Conditional jump to the relative address specified by the signed 8-bit operand
    /// following the opcode, depending on the condition NC. Note that the operand
    /// (relative address offset) is read even when the condition is false (2/3 machine
    /// cycles).
    fn op_jp_nc_r8(&mut self) {
        let offset = self.fetch_u8() as i8;

        if (self.registers.status_flags() & status_flag::CARRY) == 0 {
            self.dummy_cycle();
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
        self.write(address, self.registers.a());
        self.registers.set_hl(address.wrapping_sub(1));
    }

    /// Opcode 0x33: [INC SP](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=72)
    ///
    /// Increments data in the 16-bit register SP (2 machine cycles).
    fn op_inc_sp(&mut self) {
        self.dummy_cycle();
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
    }

    /// Opcode 0x34: [INC (HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=56)
    ///
    /// Increments data at the absolute address specified by the 16-bit register HL (3
    /// machine cycles).
    fn op_inc_hl_ind(&mut self) {
        let address = self.registers.hl();
        let value = self.read(address);
        let value = self.run_inc_u8_and_update_flags(value);
        self.write(address, value);
    }

    /// Opcode 0x35: [DEC (HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=58)
    ///
    /// Decrements data at the absolute address specified by the 16-bit register HL (3
    /// machine cycles).
    fn op_dec_hl_ind(&mut self) {
        let address = self.registers.hl();
        let value = self.read(address);
        let value = self.run_dec_u8_and_update_flags(value);
        self.write(address, value);
    }

    /// Opcode 0x36: [LD (HL),d8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=19)
    ///
    /// Load to the absolute address specified by the 16-bit register HL, the immediate
    /// data following the opcode (3 machine cycles).
    fn op_ld_hl_d8(&mut self) {
        let value = self.fetch_u8();
        self.write(self.registers.hl(), value);
    }

    /// Opcode 0x37: [SCF](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=69)
    ///
    /// Sets the carry flag, and clears the N and H flags (1 machine cycle).
    fn op_scf(&mut self) {
        self.registers.update_status_flags(status_flag::CARRY);
        self.registers
            .remove_status_flags(status_flag::HALF_CARRY | status_flag::NEGATIVE);
    }

    /// Opcode 0x38: [JR C,r8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=109)
    ///
    /// Conditional jump to the relative address specified by the signed 8-bit operand
    /// following the opcode, depending on the condition C. Note that the operand
    /// (relative address offset) is read even when the condition is false (2/3 machine
    /// cycles).
    fn op_jp_c_r8(&mut self) {
        let offset = self.fetch_u8() as i8;

        if (self.registers.status_flags() & status_flag::CARRY) != 0 {
            self.dummy_cycle();
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
        let value = self.read(address);
        self.registers.set_a(value);
        self.registers.set_hl(address.wrapping_sub(1));
    }

    /// Opcode 0x3B: [DEC SP](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=73)
    ///
    /// DEcrements data in the 16-bit register SP (2 machine cycles).
    fn op_dec_sp(&mut self) {
        self.dummy_cycle();
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    /// Opcode 0x3C: [INC A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=55)
    ///
    /// Increments data in the 8-bit register A (1 machine cycle).
    fn op_inc_a(&mut self) {
        let value = self.run_inc_u8_and_update_flags(self.registers.a());
        self.registers.set_a(value);
    }

    /// Opcode 0x3D: [DEC A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=57)
    ///
    /// Decrements data in the 8-bit register A (1 machine cycle).
    fn op_dec_a(&mut self) {
        let value = self.run_dec_u8_and_update_flags(self.registers.a());
        self.registers.set_a(value);
    }

    /// Opcode 0x3E: [LD A,d8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=16)
    ///
    /// Load to the 8-bit register A, the immediate data following the opcode (2
    /// machine cycles).
    fn op_ld_a_d8(&mut self) {
        let value = self.fetch_u8();
        self.registers.set_a(value);
    }

    /// Opcode 0x3F: [CCF](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=68)
    ///
    /// Flips the carry flag, and clears the N and H flags (1 machine cycle).
    fn op_ccf(&mut self) {
        self.registers
            .set_status_flags(self.registers.status_flags() ^ status_flag::CARRY);
        self.registers
            .remove_status_flags(status_flag::HALF_CARRY | status_flag::NEGATIVE);
    }

    /// Opcode 0x40: [LD B,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register B, data from the 8-bit register B (1 machine cycle).
    fn op_ld_b_b(&mut self) {}

    /// Opcode 0x41: [LD B,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register B, data from the 8-bit register C (1 machine cycle).
    fn op_ld_b_c(&mut self) {
        self.registers.set_b(self.registers.c());
    }

    /// Opcode 0x42: [LD B,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register B, data from the 8-bit register D (1 machine cycle).
    fn op_ld_b_d(&mut self) {
        self.registers.set_b(self.registers.d());
    }

    /// Opcode 0x43: [LD B,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register B, data from the 8-bit register E (1 machine cycle).
    fn op_ld_b_e(&mut self) {
        self.registers.set_b(self.registers.e());
    }

    /// Opcode 0x44: [LD B,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register B, data from the 8-bit register H (1 machine cycle).
    fn op_ld_b_h(&mut self) {
        self.registers.set_b(self.registers.h());
    }

    /// Opcode 0x45: [LD B,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register B, data from the 8-bit register L (1 machine cycle).
    fn op_ld_b_l(&mut self) {
        self.registers.set_b(self.registers.l());
    }

    /// Opcode 0x46: [LD B,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=17)
    ///
    /// Load to the 8-bit register B, data from the absolute address specified by the
    /// 16-bit register HL (2 machine cycle).
    fn op_ld_b_hl(&mut self) {
        let value = self.read_hl();
        self.registers.set_b(value);
    }

    /// Opcode 0x47: [LD B,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register B, data from the 8-bit register A (1 machine cycle).
    fn op_ld_b_a(&mut self) {
        self.registers.set_b(self.registers.a());
    }

    /// Opcode 0x48: [LD C,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register C, data from the 8-bit register B (1 machine cycle).
    fn op_ld_c_b(&mut self) {
        self.registers.set_c(self.registers.b());
    }

    /// Opcode 0x49: [LD C,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register C, data from the 8-bit register C (1 machine cycle).
    fn op_ld_c_c(&mut self) {}

    /// Opcode 0x4A: [LD C,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register C, data from the 8-bit register D (1 machine cycle).
    fn op_ld_c_d(&mut self) {
        self.registers.set_c(self.registers.d());
    }

    /// Opcode 0x4B: [LD C,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register C, data from the 8-bit register E (1 machine cycle).
    fn op_ld_c_e(&mut self) {
        self.registers.set_c(self.registers.e());
    }

    /// Opcode 0x4C: [LD C,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register C, data from the 8-bit register H (1 machine cycle).
    fn op_ld_c_h(&mut self) {
        self.registers.set_c(self.registers.h());
    }

    /// Opcode 0x4D: [LD C,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register C, data from the 8-bit register L (1 machine cycle).
    fn op_ld_c_l(&mut self) {
        self.registers.set_c(self.registers.l());
    }

    /// Opcode 0x4E: [LD C,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=17)
    ///
    /// Load to the 8-bit register C, data from the absolute address specified by the
    /// 16-bit register HL (2 machine cycle).
    fn op_ld_c_hl(&mut self) {
        let value = self.read_hl();
        self.registers.set_c(value);
    }

    /// Opcode 0x4F: [LD C,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register C, data from the 8-bit register A (1 machine cycle).
    fn op_ld_c_a(&mut self) {
        self.registers.set_c(self.registers.a());
    }

    /// Opcode 0x50: [LD D,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register D, data from the 8-bit register B (1 machine cycle).
    fn op_ld_d_b(&mut self) {
        self.registers.set_d(self.registers.b());
    }

    /// Opcode 0x51: [LD D,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register D, data from the 8-bit register C (1 machine cycle).
    fn op_ld_d_c(&mut self) {
        self.registers.set_d(self.registers.c());
    }

    /// Opcode 0x52: [LD D,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register D, data from the 8-bit register D (1 machine cycle).
    fn op_ld_d_d(&mut self) {}

    /// Opcode 0x53: [LD D,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register D, data from the 8-bit register E (1 machine cycle).
    fn op_ld_d_e(&mut self) {
        self.registers.set_d(self.registers.e());
    }

    /// Opcode 0x54: [LD D,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register D, data from the 8-bit register H (1 machine cycle).
    fn op_ld_d_h(&mut self) {
        self.registers.set_d(self.registers.h());
    }

    /// Opcode 0x55: [LD D,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register D, data from the 8-bit register L (1 machine cycle).
    fn op_ld_d_l(&mut self) {
        self.registers.set_d(self.registers.l());
    }

    /// Opcode 0x56: [LD D,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=17)
    ///
    /// Load to the 8-bit register D, data from the absolute address specified by the
    /// 16-bit register HL (2 machine cycle).
    fn op_ld_d_hl(&mut self) {
        let value = self.read_hl();
        self.registers.set_d(value);
    }

    /// Opcode 0x57: [LD D,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register D, data from the 8-bit register A (1 machine cycle).
    fn op_ld_d_a(&mut self) {
        self.registers.set_d(self.registers.a());
    }

    /// Opcode 0x58: [LD E,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register E, data from the 8-bit register B (1 machine cycle).
    fn op_ld_e_b(&mut self) {
        self.registers.set_e(self.registers.b());
    }

    /// Opcode 0x59: [LD E,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register E, data from the 8-bit register C (1 machine cycle).
    fn op_ld_e_c(&mut self) {
        self.registers.set_e(self.registers.c());
    }

    /// Opcode 0x5A: [LD E,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register E, data from the 8-bit register D (1 machine cycle).
    fn op_ld_e_d(&mut self) {
        self.registers.set_e(self.registers.d());
    }

    /// Opcode 0x5B: [LD E,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register E, data from the 8-bit register E (1 machine cycle).
    fn op_ld_e_e(&mut self) {}

    /// Opcode 0x5C: [LD E,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register E, data from the 8-bit register H (1 machine cycle).
    fn op_ld_e_h(&mut self) {
        self.registers.set_e(self.registers.h());
    }

    /// Opcode 0x5D: [LD E,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register E, data from the 8-bit register L (1 machine cycle).
    fn op_ld_e_l(&mut self) {
        self.registers.set_e(self.registers.l());
    }

    /// Opcode 0x5E: [LD E,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=17)
    ///
    /// Load to the 8-bit register E, data from the absolute address specified by the
    /// 16-bit register HL (2 machine cycle).
    fn op_ld_e_hl(&mut self) {
        let value = self.read_hl();
        self.registers.set_e(value);
    }

    /// Opcode 0x5F: [LD E,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register E, data from the 8-bit register A (1 machine cycle).
    fn op_ld_e_a(&mut self) {
        self.registers.set_e(self.registers.a());
    }

    /// Opcode 0x60: [LD H,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register H, data from the 8-bit register B (1 machine cycle).
    fn op_ld_h_b(&mut self) {
        self.registers.set_h(self.registers.b());
    }

    /// Opcode 0x61: [LD H,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register H, data from the 8-bit register C (1 machine cycle).
    fn op_ld_h_c(&mut self) {
        self.registers.set_h(self.registers.c());
    }

    /// Opcode 0x62: [LD H,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register H, data from the 8-bit register D (1 machine cycle).
    fn op_ld_h_d(&mut self) {
        self.registers.set_h(self.registers.d());
    }

    /// Opcode 0x63: [LD H,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register H, data from the 8-bit register E (1 machine cycle).
    fn op_ld_h_e(&mut self) {
        self.registers.set_h(self.registers.e());
    }

    /// Opcode 0x64: [LD H,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register H, data from the 8-bit register H (1 machine cycle).
    fn op_ld_h_h(&mut self) {}

    /// Opcode 0x65: [LD H,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register H, data from the 8-bit register L (1 machine cycle).
    fn op_ld_h_l(&mut self) {
        self.registers.set_h(self.registers.l());
    }

    /// Opcode 0x66: [LD H,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=17)
    ///
    /// Load to the 8-bit register H, data from the absolute address specified by the
    /// 16-bit register HL (2 machine cycle).
    fn op_ld_h_hl(&mut self) {
        let value = self.read_hl();
        self.registers.set_h(value);
    }

    /// Opcode 0x67: [LD H,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register H, data from the 8-bit register A (1 machine cycle).
    fn op_ld_h_a(&mut self) {
        self.registers.set_h(self.registers.a());
    }

    /// Opcode 0x68: [LD L,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register L, data from the 8-bit register B (1 machine cycle).
    fn op_ld_l_b(&mut self) {
        self.registers.set_l(self.registers.b());
    }

    /// Opcode 0x69: [LD L,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register L, data from the 8-bit register C (1 machine cycle).
    fn op_ld_l_c(&mut self) {
        self.registers.set_l(self.registers.c());
    }

    /// Opcode 0x6A: [LD L,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register L, data from the 8-bit register D (1 machine cycle).
    fn op_ld_l_d(&mut self) {
        self.registers.set_l(self.registers.d());
    }

    /// Opcode 0x6B: [LD L,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register L, data from the 8-bit register E (1 machine cycle).
    fn op_ld_l_e(&mut self) {
        self.registers.set_l(self.registers.e());
    }

    /// Opcode 0x6C: [LD L,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register L, data from the 8-bit register H (1 machine cycle).
    fn op_ld_l_h(&mut self) {
        self.registers.set_l(self.registers.h());
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
        let value = self.read_hl();
        self.registers.set_l(value);
    }

    /// Opcode 0x6F: [LD L,A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register L, data from the 8-bit register A (1 machine cycle).
    fn op_ld_l_a(&mut self) {
        self.registers.set_l(self.registers.a());
    }

    /// Opcode 0x70: [LD (HL),B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the absolute address specified by the 16-bit register HL, data from the
    /// 8-bit register B (2 machine cycles).
    fn op_ld_hl_b(&mut self) {
        self.write_hl(self.registers.b());
    }

    /// Opcode 0x71: [LD (HL),C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the absolute address specified by the 16-bit register HL, data from the
    /// 8-bit register C (2 machine cycles).
    fn op_ld_hl_c(&mut self) {
        self.write_hl(self.registers.c());
    }

    /// Opcode 0x72: [LD (HL),D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the absolute address specified by the 16-bit register HL, data from the
    /// 8-bit register D (2 machine cycles).
    fn op_ld_hl_d(&mut self) {
        self.write_hl(self.registers.d());
    }

    /// Opcode 0x73: [LD (HL),E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the absolute address specified by the 16-bit register HL, data from the
    /// 8-bit register E (2 machine cycles).
    fn op_ld_hl_e(&mut self) {
        self.write_hl(self.registers.e());
    }

    /// Opcode 0x74: [LD (HL),H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the absolute address specified by the 16-bit register HL, data from the
    /// 8-bit register H (2 machine cycles).
    fn op_ld_hl_h(&mut self) {
        self.write_hl(self.registers.h());
    }

    /// Opcode 0x75: [LD (HL),L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the absolute address specified by the 16-bit register HL, data from the
    /// 8-bit register L (2 machine cycles).
    fn op_ld_hl_l(&mut self) {
        self.write_hl(self.registers.l());
    }

    /// Opcode 0x75: [HALT](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=118)
    fn op_halt(&mut self) {
        self.halted = true;
    }

    /// Opcode 0x77: [LD (HL),A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the absolute address specified by the 16-bit register HL, data from the
    /// 8-bit register A (2 machine cycles).
    fn op_ld_hl_a(&mut self) {
        self.write_hl(self.registers.a());
    }

    /// Opcode 0x78: [LD A,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register A, data from the 8-bit register B (1 machine cycle).
    fn op_ld_a_b(&mut self) {
        self.registers.set_a(self.registers.b());
    }

    /// Opcode 0x79: [LD A,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register A, data from the 8-bit register C (1 machine cycle).
    fn op_ld_a_c(&mut self) {
        self.registers.set_a(self.registers.c());
    }

    /// Opcode 0x7A: [LD A,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register A, data from the 8-bit register D (1 machine cycle).
    fn op_ld_a_d(&mut self) {
        self.registers.set_a(self.registers.d());
    }

    /// Opcode 0x7B: [LD A,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register A, data from the 8-bit register E (1 machine cycle).
    fn op_ld_a_e(&mut self) {
        self.registers.set_a(self.registers.e());
    }

    /// Opcode 0x7C: [LD A,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register A, data from the 8-bit register H (1 machine cycle).
    fn op_ld_a_h(&mut self) {
        self.registers.set_a(self.registers.h());
    }

    /// Opcode 0x7D: [LD A,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=15)
    ///
    /// Load to the 8-bit register A, data from the 8-bit register L (1 machine cycle).
    fn op_ld_a_l(&mut self) {
        self.registers.set_a(self.registers.l());
    }

    /// Opcode 0x7E: [LD A,(HL)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=17)
    ///
    /// Load to the 8-bit register A, data from the absolute address specified by the
    /// 16-bit register HL (2 machine cycle).
    fn op_ld_a_hl(&mut self) {
        let value = self.read_hl();
        self.registers.set_a(value);
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
        self.run_add_u8_and_update_flags(self.registers.b());
    }

    /// Opcode 0x81: [ADD A,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=40)
    ///
    /// Adds to the 8-bit A register, the 8-bit register C, and stores the result back
    /// into the A register (1 machine cycle).
    fn op_add_a_c(&mut self) {
        self.run_add_u8_and_update_flags(self.registers.c());
    }

    /// Opcode 0x82: [ADD A,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=40)
    ///
    /// Adds to the 8-bit A register, the 8-bit register D, and stores the result back
    /// into the A register (1 machine cycle).
    fn op_add_a_d(&mut self) {
        self.run_add_u8_and_update_flags(self.registers.d());
    }

    /// Opcode 0x83: [ADD A,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=40)
    ///
    /// Adds to the 8-bit A register, the 8-bit register E, and stores the result back
    /// into the A register (1 machine cycle).
    fn op_add_a_e(&mut self) {
        self.run_add_u8_and_update_flags(self.registers.e());
    }

    /// Opcode 0x84: [ADD A,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=40)
    ///
    /// Adds to the 8-bit A register, the 8-bit register H, and stores the result back
    /// into the A register (1 machine cycle).
    fn op_add_a_h(&mut self) {
        self.run_add_u8_and_update_flags(self.registers.h());
    }

    /// Opcode 0x85: [ADD A,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=40)
    ///
    /// Adds to the 8-bit A register, the 8-bit register L, and stores the result back
    /// into the A register (1 machine cycle).
    fn op_add_a_l(&mut self) {
        self.run_add_u8_and_update_flags(self.registers.l());
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
        self.run_add_u8_and_update_flags(self.registers.a());
    }

    /// Opcode 0x88: [ADC A,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=43)
    ///
    /// Adds to the 8-bit A register, the carry flag and the 8-bit register b, and
    /// stores the result back into the A register (1 machine cycle).
    fn op_adc_a_b(&mut self) {
        self.run_adc_and_update_flags(self.registers.b());
    }

    /// Opcode 0x89: [ADC A,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=43)
    ///
    /// Adds to the 8-bit A register, the carry flag and the 8-bit register C, and
    /// stores the result back into the A register (1 machine cycle).
    fn op_adc_a_c(&mut self) {
        self.run_adc_and_update_flags(self.registers.c());
    }

    /// Opcode 0x8A: [ADC A,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=43)
    ///
    /// Adds to the 8-bit A register, the carry flag and the 8-bit register D, and
    /// stores the result back into the A register (1 machine cycle).
    fn op_adc_a_d(&mut self) {
        self.run_adc_and_update_flags(self.registers.d());
    }

    /// Opcode 0x8B: [ADC A,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=43)
    ///
    /// Adds to the 8-bit A register, the carry flag and the 8-bit register E, and
    /// stores the result back into the A register (1 machine cycle).
    fn op_adc_a_e(&mut self) {
        self.run_adc_and_update_flags(self.registers.e());
    }

    /// Opcode 0x8C: [ADC A,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=43)
    ///
    /// Adds to the 8-bit A register, the carry flag and the 8-bit register H, and
    /// stores the result back into the A register (1 machine cycle).
    fn op_adc_a_h(&mut self) {
        self.run_adc_and_update_flags(self.registers.h());
    }

    /// Opcode 0x8D: [ADC A,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=43)
    ///
    /// Adds to the 8-bit A register, the carry flag and the 8-bit register L, and
    /// stores the result back into the A register (1 machine cycle).
    fn op_adc_a_l(&mut self) {
        self.run_adc_and_update_flags(self.registers.l());
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
        self.run_adc_and_update_flags(self.registers.a());
    }

    /// Opcode 0x90: [SUB B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=46)
    ///
    /// Subtracts from the 8-bit A register, the 8-bit register B, and stores the
    /// result back into the A register (1 machine cycle).
    fn op_sub_a_b(&mut self) {
        self.run_sub_and_update_flags(self.registers.b());
    }

    /// Opcode 0x91: [SUB C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=46)
    ///
    /// Subtracts from the 8-bit A register, the 8-bit register C, and stores the
    /// result back into the A register (1 machine cycle).
    fn op_sub_a_c(&mut self) {
        self.run_sub_and_update_flags(self.registers.c());
    }

    /// Opcode 0x92: [SUB D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=46)
    ///
    /// Subtracts from the 8-bit A register, the 8-bit register D, and stores the
    /// result back into the A register (1 machine cycle).
    fn op_sub_a_d(&mut self) {
        self.run_sub_and_update_flags(self.registers.d());
    }

    /// Opcode 0x93: [SUB E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=46)
    ///
    /// Subtracts from the 8-bit A register, the 8-bit register E, and stores the
    /// result back into the A register (1 machine cycle).
    fn op_sub_a_e(&mut self) {
        self.run_sub_and_update_flags(self.registers.e());
    }

    /// Opcode 0x94: [SUB H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=46)
    ///
    /// Subtracts from the 8-bit A register, the 8-bit register H, and stores the
    /// result back into the A register (1 machine cycle).
    fn op_sub_a_h(&mut self) {
        self.run_sub_and_update_flags(self.registers.h());
    }

    /// Opcode 0x95: [SUB L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=46)
    ///
    /// Subtracts from the 8-bit A register, the 8-bit register L, and stores the
    /// result back into the A register (1 machine cycle).
    fn op_sub_a_l(&mut self) {
        self.run_sub_and_update_flags(self.registers.l());
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
        self.run_sub_and_update_flags(self.registers.a());
    }

    /// Opcode 0x98: [SBC A,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=49)
    ///
    /// Subtracts from the 8-bit A register, the carry flag and the 8-bit register B,
    /// and stores the result back into the A register (1 machine cycle).
    fn op_sbc_a_b(&mut self) {
        self.run_sbc_and_update_flags(self.registers.b());
    }

    /// Opcode 0x99: [SBC A,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=49)
    ///
    /// Subtracts from the 8-bit A register, the carry flag and the 8-bit register C,
    /// and stores the result back into the A register (1 machine cycle).
    fn op_sbc_a_c(&mut self) {
        self.run_sbc_and_update_flags(self.registers.c());
    }

    /// Opcode 0x9A: [SBC A,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=49)
    ///
    /// Subtracts from the 8-bit A register, the carry flag and the 8-bit register D,
    /// and stores the result back into the A register (1 machine cycle).
    fn op_sbc_a_d(&mut self) {
        self.run_sbc_and_update_flags(self.registers.d());
    }

    /// Opcode 0x9B: [SBC A,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=49)
    ///
    /// Subtracts from the 8-bit A register, the carry flag and the 8-bit register E,
    /// and stores the result back into the A register (1 machine cycle).
    fn op_sbc_a_e(&mut self) {
        self.run_sbc_and_update_flags(self.registers.e());
    }

    /// Opcode 0x9C: [SBC A,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=49)
    ///
    /// Subtracts from the 8-bit A register, the carry flag and the 8-bit register H,
    /// and stores the result back into the A register (1 machine cycle).
    fn op_sbc_a_h(&mut self) {
        self.run_sbc_and_update_flags(self.registers.h());
    }

    /// Opcode 0x9D: [SBC A,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=49)
    ///
    /// Subtracts from the 8-bit A register, the carry flag and the 8-bit register L,
    /// and stores the result back into the A register (1 machine cycle).
    fn op_sbc_a_l(&mut self) {
        self.run_sbc_and_update_flags(self.registers.l());
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
        self.run_sbc_and_update_flags(self.registers.a());
    }

    /// Opcode 0xA0: [AND B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=59)
    ///
    /// Performs a bitwise AND operation between the 8-bit A register and the 8-bit
    /// register B, and stores the result back into the A register (1 machine cycle).
    fn op_and_a_b(&mut self) {
        self.run_and_and_update_flags(self.registers.b());
    }

    /// Opcode 0xA1: [AND C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=59)
    ///
    /// Performs a bitwise AND operation between the 8-bit A register and the 8-bit
    /// register C, and stores the result back into the A register (1 machine cycle).
    fn op_and_a_c(&mut self) {
        self.run_and_and_update_flags(self.registers.c());
    }

    /// Opcode 0xA2: [AND D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=59)
    ///
    /// Performs a bitwise AND operation between the 8-bit A register and the 8-bit
    /// register D, and stores the result back into the A register (1 machine cycle).
    fn op_and_a_d(&mut self) {
        self.run_and_and_update_flags(self.registers.d());
    }

    /// Opcode 0xA3: [AND E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=59)
    ///
    /// Performs a bitwise AND operation between the 8-bit A register and the 8-bit
    /// register E, and stores the result back into the A register (1 machine cycle).
    fn op_and_a_e(&mut self) {
        self.run_and_and_update_flags(self.registers.e());
    }

    /// Opcode 0xA4: [AND H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=59)
    ///
    /// Performs a bitwise AND operation between the 8-bit A register and the 8-bit
    /// register H, and stores the result back into the A register (1 machine cycle).
    fn op_and_a_h(&mut self) {
        self.run_and_and_update_flags(self.registers.h());
    }

    /// Opcode 0xA5: [AND L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=59)
    ///
    /// Performs a bitwise AND operation between the 8-bit A register and the 8-bit
    /// register L, and stores the result back into the A register (1 machine cycle).
    fn op_and_a_l(&mut self) {
        self.run_and_and_update_flags(self.registers.l());
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
        self.run_and_and_update_flags(self.registers.a());
    }

    /// Opcode 0xA8: [XOR B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=65)
    ///
    /// Performs a bitwise XOR operation between the 8-bit A register and the 8-bit
    /// register B, and stores the result back into the A register (1 machine cycle).
    fn op_xor_a_b(&mut self) {
        self.run_xor_and_update_flags(self.registers.b());
    }

    /// Opcode 0xA9: [XOR C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=65)
    ///
    /// Performs a bitwise XOR operation between the 8-bit A register and the 8-bit
    /// register C, and stores the result back into the A register (1 machine cycle).
    fn op_xor_a_c(&mut self) {
        self.run_xor_and_update_flags(self.registers.c());
    }

    /// Opcode 0xAA: [XOR D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=65)
    ///
    /// Performs a bitwise XOR operation between the 8-bit A register and the 8-bit
    /// register D, and stores the result back into the A register (1 machine cycle).
    fn op_xor_a_d(&mut self) {
        self.run_xor_and_update_flags(self.registers.d());
    }

    /// Opcode 0xAB: [XOR E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=65)
    ///
    /// Performs a bitwise XOR operation between the 8-bit A register and the 8-bit
    /// register E, and stores the result back into the A register (1 machine cycle).
    fn op_xor_a_e(&mut self) {
        self.run_xor_and_update_flags(self.registers.e());
    }

    /// Opcode 0xAC: [XOR H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=65)
    ///
    /// Performs a bitwise XOR operation between the 8-bit A register and the 8-bit
    /// register H, and stores the result back into the A register (1 machine cycle).
    fn op_xor_a_h(&mut self) {
        self.run_xor_and_update_flags(self.registers.h());
    }

    /// Opcode 0xAD: [XOR L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=65)
    ///
    /// Performs a bitwise XOR operation between the 8-bit A register and the 8-bit
    /// register L, and stores the result back into the A register (1 machine cycle).
    fn op_xor_a_l(&mut self) {
        self.run_xor_and_update_flags(self.registers.l());
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
        self.run_xor_and_update_flags(self.registers.a());
    }

    /// Opcode 0xB0: [OR B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=62)
    ///
    /// Performs a bitwise OR operation between the 8-bit A register and the 8-bit
    /// register B, and stores the result back into the A register (1 machine cycle).
    fn op_or_a_b(&mut self) {
        self.run_or_and_update_flags(self.registers.b());
    }

    /// Opcode 0xB1: [OR C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=62)
    ///
    /// Performs a bitwise OR operation between the 8-bit A register and the 8-bit
    /// register C, and stores the result back into the A register (1 machine cycle).
    fn op_or_a_c(&mut self) {
        self.run_or_and_update_flags(self.registers.c());
    }

    /// Opcode 0xB2: [OR D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=62)
    ///
    /// Performs a bitwise OR operation between the 8-bit A register and the 8-bit
    /// register D, and stores the result back into the A register (1 machine cycle).
    fn op_or_a_d(&mut self) {
        self.run_or_and_update_flags(self.registers.d());
    }

    /// Opcode 0xB3: [OR E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=62)
    ///
    /// Performs a bitwise OR operation between the 8-bit A register and the 8-bit
    /// register E, and stores the result back into the A register (1 machine cycle).
    fn op_or_a_e(&mut self) {
        self.run_or_and_update_flags(self.registers.e());
    }

    /// Opcode 0xB4: [OR H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=62)
    ///
    /// Performs a bitwise OR operation between the 8-bit A register and the 8-bit
    /// register h, and stores the result back into the A register (1 machine cycle).
    fn op_or_a_h(&mut self) {
        self.run_or_and_update_flags(self.registers.h());
    }

    /// Opcode 0xB5: [OR L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=62)
    ///
    /// Performs a bitwise OR operation between the 8-bit A register and the 8-bit
    /// register L, and stores the result back into the A register (1 machine cycle).
    fn op_or_a_l(&mut self) {
        self.run_or_and_update_flags(self.registers.l());
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
        self.run_or_and_update_flags(self.registers.a());
    }

    /// Opcode 0xB8: [CP B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=52)
    ///
    /// Subtracts from the 8-bit A register, the 8-bit register B, and updates flags
    /// based on the result. This instruction is basically identical to SUB B, but does
    /// not update the A register (1 machine cycle).
    fn op_cp_a_b(&mut self) {
        self.run_cp_and_update_flags(self.registers.b());
    }

    /// Opcode 0xB9: [CP C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=52)
    ///
    /// Subtracts from the 8-bit A register, the 8-bit register C, and updates flags
    /// based on the result. This instruction is basically identical to SUB C, but does
    /// not update the A register (1 machine cycle).
    fn op_cp_a_c(&mut self) {
        self.run_cp_and_update_flags(self.registers.c());
    }

    /// Opcode 0xBA: [CP D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=52)
    ///
    /// Subtracts from the 8-bit A register, the 8-bit register D, and updates flags
    /// based on the result. This instruction is basically identical to SUB D, but does
    /// not update the A register (1 machine cycle).
    fn op_cp_a_d(&mut self) {
        self.run_cp_and_update_flags(self.registers.d());
    }

    /// Opcode 0xBB: [CP E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=52)
    ///
    /// Subtracts from the 8-bit A register, the 8-bit register E, and updates flags
    /// based on the result. This instruction is basically identical to SUB E, but does
    /// not update the A register (1 machine cycle).
    fn op_cp_a_e(&mut self) {
        self.run_cp_and_update_flags(self.registers.e());
    }

    /// Opcode 0xBC: [CP H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=52)
    ///
    /// Subtracts from the 8-bit A register, the 8-bit register H, and updates flags
    /// based on the result. This instruction is basically identical to SUB H, but does
    /// not update the A register (1 machine cycle).
    fn op_cp_a_h(&mut self) {
        self.run_cp_and_update_flags(self.registers.h());
    }

    /// Opcode 0xBD: [CP L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=52)
    ///
    /// Subtracts from the 8-bit A register, the 8-bit register L, and updates flags
    /// based on the result. This instruction is basically identical to SUB L, but does
    /// not update the A register (1 machine cycle).
    fn op_cp_a_l(&mut self) {
        self.run_cp_and_update_flags(self.registers.l());
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
        self.run_cp_and_update_flags(self.registers.a());
    }

    /// Opcode 0xC0: [RET NZ](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=115)
    ///
    /// Conditional return from a function, depending on the condition NZ (2/5 machine
    /// cycles).
    fn op_ret_nz(&mut self) {
        self.dummy_cycle();

        if (self.registers.status_flags() & status_flag::ZERO) == 0 {
            self.dummy_cycle();
            self.program_counter = self.stack_pop();
        }
    }

    /// Opcode 0xC1: [POP BC](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=38)
    ///
    /// Pops to the 16-bit register BC, data from the stack memory (3 machine cycles).
    fn op_pop_bc(&mut self) {
        let value = self.stack_pop();
        self.registers.set_bc(value);
    }

    /// Opcode 0xC2: [JP NZ,a16](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=106)
    ///
    /// Conditional jump to the absolute address specified by the 16-bit operand
    /// following the opcode, depending on the condition NZ. Note that the operand
    /// (absolute address) is read even when the condition is false (3/4 machine
    /// cycles).
    fn op_jp_nz_a16(&mut self) {
        let address = self.fetch_u16();

        if (self.registers.status_flags() & status_flag::ZERO) == 0 {
            self.dummy_cycle();
            self.program_counter = address;
        }
    }

    /// Opcode 0xC3: [JP a16](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=104)
    ///
    /// Unconditional jump to the absolute address specified by the 16-bit immediate
    /// operand following the opcode (4 machine cycles).
    fn op_jp_a16(&mut self) {
        self.dummy_cycle();
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

        if (self.registers.status_flags() & status_flag::ZERO) == 0 {
            self.stack_push(self.program_counter);
            self.program_counter = address;
        }
    }

    /// Opcode 0xC5: [PUSH BC](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=37)
    ///
    /// Push to the stack memory, data from the 16-bit register BC (4 machine cycles).
    fn op_push_bc(&mut self) {
        self.stack_push(self.registers.bc());
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
        self.stack_push(self.program_counter);
        self.program_counter = 0x0000;
    }

    /// Opcode 0xC8: [RET Z](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=115)
    ///
    /// Conditional return from a function, depending on the condition 2 (2/5 machine
    /// cycles).
    fn op_ret_z(&mut self) {
        self.dummy_cycle();

        if (self.registers.status_flags() & status_flag::ZERO) != 0 {
            self.dummy_cycle();
            self.program_counter = self.stack_pop();
        }
    }

    /// Opcode 0xC9: [RET](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=114)
    ///
    /// Unconditional return from a function (4 machine cycles).
    fn op_ret(&mut self) {
        self.dummy_cycle();
        self.program_counter = self.stack_pop();
    }

    /// Opcode 0xCA: [JP Z,a16](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=106)
    ///
    /// Conditional jump to the absolute address specified by the 16-bit operand
    /// following the opcode, depending on the condition Z. Note that the operand
    /// (absolute address) is read even when the condition is false (3/4 machine
    /// cycles).
    fn op_jp_z_a16(&mut self) {
        let address = self.fetch_u16();

        if (self.registers.status_flags() & status_flag::ZERO) != 0 {
            self.dummy_cycle();
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

        if (self.registers.status_flags() & status_flag::ZERO) != 0 {
            self.stack_push(self.program_counter);
            self.program_counter = address;
        }
    }

    /// Opcode 0xCD: [CALL a16](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=111)
    ///
    /// Unconditional function call to the absolute address specified by the 16-bit
    /// operand following the opcode (6 machine cycles).
    fn op_call_a16(&mut self) {
        let address = self.fetch_u16();
        self.stack_push(self.program_counter);
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
        self.stack_push(self.program_counter);
        self.program_counter = 0x0008;
    }

    /// Opcode 0xD0: [RET NC](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=115)
    ///
    /// Conditional return from a function, depending on the condition NC (2/5 machine
    /// cycles).
    fn op_ret_nc(&mut self) {
        self.dummy_cycle();

        if (self.registers.status_flags() & status_flag::CARRY) == 0 {
            self.dummy_cycle();
            self.program_counter = self.stack_pop();
        }
    }

    /// Opcode 0xD1: [POP DE](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=38)
    ///
    /// Pops to the 16-bit register DE, data from the stack memory (3 machine cycles).
    fn op_pop_de(&mut self) {
        let value = self.stack_pop();
        self.registers.set_de(value);
    }

    /// Opcode 0xD2: [JP NC,a16](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=106)
    ///
    /// Conditional jump to the absolute address specified by the 16-bit operand
    /// following the opcode, depending on the condition NC. Note that the operand
    /// (absolute address) is read even when the condition is false (3/4 machine
    /// cycles).
    fn op_jp_nc_a16(&mut self) {
        let address = self.fetch_u16();

        if (self.registers.status_flags() & status_flag::CARRY) == 0 {
            self.dummy_cycle();
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

        if (self.registers.status_flags() & status_flag::CARRY) == 0 {
            self.stack_push(self.program_counter);
            self.program_counter = address;
        }
    }

    /// Opcode 0xD5: [PUSH DE](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=37)
    ///
    /// Push to the stack memory, data from the 16-bit register DE (4 machine cycles).
    fn op_push_de(&mut self) {
        self.stack_push(self.registers.de());
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
        self.stack_push(self.program_counter);
        self.program_counter = 0x0010;
    }

    /// Opcode 0xD8: [RET C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=115)
    ///
    /// Conditional return from a function, depending on the condition C (2/5 machine
    /// cycles).
    fn op_ret_c(&mut self) {
        self.dummy_cycle();

        if (self.registers.status_flags() & status_flag::CARRY) != 0 {
            self.dummy_cycle();
            self.program_counter = self.stack_pop();
        }
    }

    /// Opcode 0xD9: [RETI](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=116)
    ///
    /// Unconditional return from a function. Also enables interrupts by setting IME=1
    /// (4 machine cycles).
    fn op_reti(&mut self) {
        self.dummy_cycle();
        self.program_counter = self.stack_pop();
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

        if (self.registers.status_flags() & status_flag::CARRY) != 0 {
            self.dummy_cycle();
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

        if (self.registers.status_flags() & status_flag::CARRY) != 0 {
            self.stack_push(self.program_counter);
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
        self.stack_push(self.program_counter);
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
        self.write(address, self.registers.a());
    }

    /// Opcode 0xE1: [POP HL](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=38)
    ///
    /// Pops to the 16-bit register HL, data from the stack memory (3 machine cycles).
    fn op_pop_hl(&mut self) {
        let value = self.stack_pop();
        self.registers.set_hl(value);
    }

    /// Opcode 0xE2: [LDH (C),A](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=27)
    ///
    /// Load to the address specified by the 8-bit C register, data from the 8-bit A
    /// register. The full 16-bit absolute address is obtained by setting the most
    /// significant byte to 0xFF and the least significant byte to the value of C, so
    /// the possible range is 0xFF00-0xFFFF (2 machine cycles).
    fn op_ldh_c_a(&mut self) {
        let address = 0xFF00 | self.registers.c() as u16;
        self.write(address, self.registers.a());
    }

    /// Opcode 0xE5: [PUSH HL](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=37)
    ///
    /// Push to the stack memory, data from the 16-bit register HL (4 machine cycles).
    fn op_push_hl(&mut self) {
        self.stack_push(self.registers.hl());
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
        self.stack_push(self.program_counter);
        self.program_counter = 0x0020;
    }

    /// Opcode 0xE8: [ADD SP,r8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=75)
    ///
    /// Loads to the 16-bit SP register, 16-bit data calculated by adding the signed
    /// 8-bit operand following the opcode to the 16-bit value of the SP register (4
    /// machine cycles).
    fn op_add_sp_r8(&mut self) {
        let offset = self.fetch_u8() as i8;

        let stack_pointer = (self.stack_pointer as i32).wrapping_add(offset as i32) as u16;
        self.registers.reset_status_flags();

        if ((self.stack_pointer & 0x0F).wrapping_add(offset as u16 & 0x0F)) > 0x0F {
            self.registers.update_status_flags(status_flag::HALF_CARRY);
        }

        if ((self.stack_pointer & 0xFF).wrapping_add(offset as u16 & 0xFF)) > 0xFF {
            self.registers.update_status_flags(status_flag::CARRY);
        }

        self.dummy_cycle();
        self.stack_pointer = stack_pointer;
        self.dummy_cycle();
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
        self.write(address, self.registers.a());
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
        self.stack_push(self.program_counter);
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
        let value = self.read(address);
        self.registers.set_a(value);
    }

    /// Opcode 0xF1: [POP AF](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=38)
    ///
    /// Pops to the 16-bit register AF, data from the stack memory. This instruction
    /// does not do calculations that affect flags, but POP AF completely replaces the
    /// F register value, so all flags are changed based on the 8-bit data that is read
    /// from memory (3 machine cycles).
    fn op_pop_af(&mut self) {
        let value = self.stack_pop() & 0xFFF0;
        self.registers.set_af(value);
    }

    /// Opcode 0xF2: [LDH A,(C)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=26)
    ///
    /// Load to the 8-bit A register, data from the address specified by the 8-bit C
    /// register. The full 16-bit absolute address is obtained by setting the most
    /// significant byte to 0xFF and the least significant byte to the value of C, so
    /// the possible range is 0xFF00-0xFFFF (2 machine cycles).
    fn op_ldh_a_c(&mut self) {
        let address = 0xFF00 | self.registers.c() as u16;
        let value = self.read(address);
        self.registers.set_a(value);
    }

    /// Opcode 0xF3: [DI](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=118)
    ///
    /// Disables interrupt handling by setting IME=0 and cancelling any scheduled
    /// effects of the EI instruction if any (1 machine cycles).
    fn op_di(&mut self) {
        self.interrupt_master_enabled = false;
    }

    /// Opcode 0xF5: [PUSH AF](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=37)
    ///
    /// Push to the stack memory, data from the 16-bit register AF (4 machine cycles).
    fn op_push_af(&mut self) {
        self.stack_push(self.registers.af());
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
        self.stack_push(self.program_counter);
        self.program_counter = 0x0030;
    }

    /// Opcode 0xF8: [LD HL,SP+r8](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=39)
    ///
    /// Load to the HL register, 16-bit data calculated by adding the signed 8-bit
    /// operand e to the 16-bit value of the SP register (3 machine cycles).
    fn op_ld_hl_sp_i8(&mut self) {
        let offset = self.fetch_u8() as i8;

        self.registers.reset_status_flags();
        self.registers
            .set_hl((self.stack_pointer as i32).wrapping_add(offset as i32) as u16);

        if (self.stack_pointer & 0x0F).wrapping_add(offset as u16 & 0x0F) > 0x0F {
            self.registers.update_status_flags(status_flag::HALF_CARRY);
        }

        if (self.stack_pointer & 0xFF).wrapping_add(offset as u16 & 0xFF) > 0xFF {
            self.registers.update_status_flags(status_flag::CARRY);
        }

        self.dummy_cycle();
    }

    /// Opcode 0xF9: [LD SP,HL](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=36)
    ///
    /// Load to the 16-bit SP register, data from the 16-bit HL register (2 machine
    /// cycles).
    fn op_ld_sp_hl(&mut self) {
        self.stack_pointer = self.registers.hl();
        self.dummy_cycle();
    }

    /// Opcode 0xFA: [LD A,(a16)](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=24)
    ///
    /// Load to the 8-bit A register, data from the absolute address specified by the
    /// 16-bit operand following the opcode (4 machine cycles).
    fn op_ld_a_a16(&mut self) {
        let address = self.fetch_u16();
        let value = self.read(address);
        self.registers.set_a(value);
    }

    /// Opcode 0xFB: [EI](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=119)
    ///
    /// Schedules interrupt handling to be enabled after the next machine cycle (1
    /// machine cycle).
    fn op_ei(&mut self) {
        self.interrupt_master_toggle = true;
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
        self.stack_push(self.program_counter);
        self.program_counter = 0x0038;
    }
}

pub const OP_CODE_FUNCTION_TABLE: [fn(&mut Cpu); 256] = [
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
    Cpu::op_ldh_c_a,     // 0xE2 : LDH (C),A
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
    Cpu::op_ldh_a_c,     // 0xF2 : LDH A,(C)
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

#[cfg(test)]
mod tests {
    use crate::emulator::cpu::{status_flag, Cpu};
    use crate::emulator::interrupts::InterruptEmitter;
    use crate::emulator::memory::{ConsoleMemory, Memory};
    use crate::emulator::timer;

    enum BranchCondition {
        NonZero,
        Zero,
        NonCarry,
        Carry,
    }

    impl BranchCondition {
        fn is_valid(&self, status_flag: u8) -> bool {
            match self {
                BranchCondition::NonZero => (status_flag & status_flag::ZERO) == 0,
                BranchCondition::Zero => (status_flag & status_flag::ZERO) != 0,
                BranchCondition::NonCarry => (status_flag & status_flag::CARRY) == 0,
                BranchCondition::Carry => (status_flag & status_flag::CARRY) != 0,
            }
        }
    }

    struct BranchTiming {
        timing_skipped: u8,
        timing_taken: u8,
        condition: BranchCondition,
    }

    impl BranchTiming {
        fn as_states_and_timings_iter(self) -> impl Iterator<Item = (u8, u8)> {
            (0..0xFu8).into_iter().map(move |status_flag| {
                let status_flag = status_flag << 4;

                (
                    status_flag,
                    if self.condition.is_valid(status_flag) {
                        self.timing_taken
                    } else {
                        self.timing_skipped
                    },
                )
            })
        }
    }

    enum Timing {
        Constant(u8),
        Branch(BranchTiming),
        Ignored,
    }

    const OP_CODE_TIMINGS: [Timing; 256] = [
        Timing::Constant(1), // 0x00 : NOP
        Timing::Constant(3), // 0x01 : LD BC,d16
        Timing::Constant(2), // 0x02 : LD (BC),A
        Timing::Constant(2), // 0x03 : INC BC
        Timing::Constant(1), // 0x04 : INC B
        Timing::Constant(1), // 0x05 : DEC B
        Timing::Constant(2), // 0x06 : LD B,d8
        Timing::Constant(1), // 0x07 : RLCA
        Timing::Constant(5), // 0x08 : LD (a16),SP
        Timing::Constant(2), // 0x09 : ADD HL,BC
        Timing::Constant(2), // 0x0A : LD A,(BC)
        Timing::Constant(2), // 0x0B : DEC BC
        Timing::Constant(1), // 0x0C : INC C
        Timing::Constant(1), // 0x0D : DEC C
        Timing::Constant(2), // 0x0E : LD C,d8
        Timing::Constant(1), // 0x0F : RRCA
        Timing::Ignored,     // 0x10 : STOP 0
        Timing::Constant(3), // 0x11 : LD DE,d16
        Timing::Constant(2), // 0x12 : LD (DE),A
        Timing::Constant(2), // 0x13 : INC DE
        Timing::Constant(1), // 0x14 : INC D
        Timing::Constant(1), // 0x15 : DEC D
        Timing::Constant(2), // 0x16 : LD D,d8
        Timing::Constant(1), // 0x17 : RLA
        Timing::Constant(3), // 0x18 : JR r8
        Timing::Constant(2), // 0x19 : ADD HL,DE
        Timing::Constant(2), // 0x1A : LD A,(DE)
        Timing::Constant(2), // 0x1B : DEC DE
        Timing::Constant(1), // 0x1C : INC E
        Timing::Constant(1), // 0x1D : DEC E
        Timing::Constant(2), // 0x1E : LD E,d8
        Timing::Constant(1), // 0x1F : RRA
        Timing::Branch(BranchTiming {
            timing_skipped: 2,
            timing_taken: 3,
            condition: BranchCondition::NonZero,
        }), // 0x20 : JR NZ,r8
        Timing::Constant(3), // 0x21 : LD HL,d16
        Timing::Constant(2), // 0x22 : LD (HL+),A
        Timing::Constant(2), // 0x23 : INC HL
        Timing::Constant(1), // 0x24 : INC H
        Timing::Constant(1), // 0x25 : DEC H
        Timing::Constant(2), // 0x26 : LD H,d8
        Timing::Constant(1), // 0x27 : DAA
        Timing::Branch(BranchTiming {
            timing_skipped: 2,
            timing_taken: 3,
            condition: BranchCondition::Zero,
        }), // 0x28 : JR Z,r8
        Timing::Constant(2), // 0x29 : ADD HL,HL
        Timing::Constant(2), // 0x2A : LD A,(HL+)
        Timing::Constant(2), // 0x2B : DEC HL
        Timing::Constant(1), // 0x2C : INC L
        Timing::Constant(1), // 0x2D : DEC L
        Timing::Constant(2), // 0x2E : LD L,d8
        Timing::Constant(1), // 0x2F : CPL
        Timing::Branch(BranchTiming {
            timing_skipped: 2,
            timing_taken: 3,
            condition: BranchCondition::NonCarry,
        }), // 0x30 : JR NC,r8
        Timing::Constant(3), // 0x31 : LD SP,d16
        Timing::Constant(2), // 0x32 : LD (HL-),A
        Timing::Constant(2), // 0x33 : INC SP
        Timing::Constant(3), // 0x34 : INC (HL)
        Timing::Constant(3), // 0x35 : DEC (HL)
        Timing::Constant(3), // 0x36 : LD (HL),d8
        Timing::Constant(1), // 0x37 : SCF
        Timing::Branch(BranchTiming {
            timing_skipped: 2,
            timing_taken: 3,
            condition: BranchCondition::Carry,
        }), // 0x38 : JR C,r8
        Timing::Constant(2), // 0x39 : ADD HL,SP
        Timing::Constant(2), // 0x3A : LD A,(HL-)
        Timing::Constant(2), // 0x3B : DEC SP
        Timing::Constant(1), // 0x3C : INC A
        Timing::Constant(1), // 0x3D : DEC A
        Timing::Constant(2), // 0x3E : LD A,d8
        Timing::Constant(1), // 0x3F : CCF
        Timing::Constant(1), // 0x40 : LD B,B
        Timing::Constant(1), // 0x41 : LD B,C
        Timing::Constant(1), // 0x42 : LD B,D
        Timing::Constant(1), // 0x43 : LD B,E
        Timing::Constant(1), // 0x44 : LD B,H
        Timing::Constant(1), // 0x45 : LD B,L
        Timing::Constant(2), // 0x46 : LD B,(HL)
        Timing::Constant(1), // 0x47 : LD B,A
        Timing::Constant(1), // 0x48 : LD C,B
        Timing::Constant(1), // 0x49 : LD C,C
        Timing::Constant(1), // 0x4A : LD C,D
        Timing::Constant(1), // 0x4B : LD C,E
        Timing::Constant(1), // 0x4C : LD C,H
        Timing::Constant(1), // 0x4D : LD C,L
        Timing::Constant(2), // 0x4E : LD C,(HL)
        Timing::Constant(1), // 0x4F : LD C,A
        Timing::Constant(1), // 0x50 : LD D,B
        Timing::Constant(1), // 0x51 : LD D,C
        Timing::Constant(1), // 0x52 : LD D,D
        Timing::Constant(1), // 0x53 : LD D,E
        Timing::Constant(1), // 0x54 : LD D,H
        Timing::Constant(1), // 0x55 : LD D,L
        Timing::Constant(2), // 0x56 : LD D,(HL)
        Timing::Constant(1), // 0x57 : LD D,A
        Timing::Constant(1), // 0x58 : LD E,B
        Timing::Constant(1), // 0x59 : LD E,C
        Timing::Constant(1), // 0x5A : LD E,D
        Timing::Constant(1), // 0x5B : LD E,E
        Timing::Constant(1), // 0x5C : LD E,H
        Timing::Constant(1), // 0x5D : LD E,L
        Timing::Constant(2), // 0x5E : LD E,(HL)
        Timing::Constant(1), // 0x5F : LD E,A
        Timing::Constant(1), // 0x60 : LD H,B
        Timing::Constant(1), // 0x61 : LD H,C
        Timing::Constant(1), // 0x62 : LD H,D
        Timing::Constant(1), // 0x63 : LD H,E
        Timing::Constant(1), // 0x64 : LD H,H
        Timing::Constant(1), // 0x65 : LD H,L
        Timing::Constant(2), // 0x66 : LD H,(HL)
        Timing::Constant(1), // 0x67 : LD H,A
        Timing::Constant(1), // 0x68 : LD L,B
        Timing::Constant(1), // 0x69 : LD L,C
        Timing::Constant(1), // 0x6A : LD L,D
        Timing::Constant(1), // 0x6B : LD L,E
        Timing::Constant(1), // 0x6C : LD L,H
        Timing::Constant(1), // 0x6D : LD L,L
        Timing::Constant(2), // 0x6E : LD L,(HL)
        Timing::Constant(1), // 0x6F : LD L,A
        Timing::Constant(2), // 0x70 : LD (HL),B
        Timing::Constant(2), // 0x71 : LD (HL),C
        Timing::Constant(2), // 0x72 : LD (HL),D
        Timing::Constant(2), // 0x73 : LD (HL),E
        Timing::Constant(2), // 0x74 : LD (HL),H
        Timing::Constant(2), // 0x75 : LD (HL),L
        Timing::Ignored,     // 0x76 : HALT
        Timing::Constant(2), // 0x77 : LD (HL),A
        Timing::Constant(1), // 0x78 : LD A,B
        Timing::Constant(1), // 0x79 : LD A,C
        Timing::Constant(1), // 0x7A : LD A,D
        Timing::Constant(1), // 0x7B : LD A,E
        Timing::Constant(1), // 0x7C : LD A,H
        Timing::Constant(1), // 0x7D : LD A,L
        Timing::Constant(2), // 0x7E : LD A,(HL)
        Timing::Constant(1), // 0x7F : LD A,A
        Timing::Constant(1), // 0x80 : ADD A,B
        Timing::Constant(1), // 0x81 : ADD A,C
        Timing::Constant(1), // 0x82 : ADD A,D
        Timing::Constant(1), // 0x83 : ADD A,E
        Timing::Constant(1), // 0x84 : ADD A,H
        Timing::Constant(1), // 0x85 : ADD A,L
        Timing::Constant(2), // 0x86 : ADD A,(HL)
        Timing::Constant(1), // 0x87 : ADD A,A
        Timing::Constant(1), // 0x88 : ADC A,B
        Timing::Constant(1), // 0x89 : ADC A,C
        Timing::Constant(1), // 0x8A : ADC A,D
        Timing::Constant(1), // 0x8B : ADC A,E
        Timing::Constant(1), // 0x8C : ADC A,H
        Timing::Constant(1), // 0x8D : ADC A,L
        Timing::Constant(2), // 0x8E : ADC A,(HL)
        Timing::Constant(1), // 0x8F : ADC A,A
        Timing::Constant(1), // 0x90 : SUB B
        Timing::Constant(1), // 0x91 : SUB C
        Timing::Constant(1), // 0x92 : SUB D
        Timing::Constant(1), // 0x93 : SUB E
        Timing::Constant(1), // 0x94 : SUB H
        Timing::Constant(1), // 0x95 : SUB L
        Timing::Constant(2), // 0x96 : SUB (HL)
        Timing::Constant(1), // 0x97 : SUB A
        Timing::Constant(1), // 0x98 : SBC A,B
        Timing::Constant(1), // 0x99 : SBC A,C
        Timing::Constant(1), // 0x9A : SBC A,D
        Timing::Constant(1), // 0x9B : SBC A,E
        Timing::Constant(1), // 0x9C : SBC A,H
        Timing::Constant(1), // 0x9D : SBC A,L
        Timing::Constant(2), // 0x9E : SBC A,(HL)
        Timing::Constant(1), // 0x9F : SBC A,A
        Timing::Constant(1), // 0xA0 : AND B
        Timing::Constant(1), // 0xA1 : AND C
        Timing::Constant(1), // 0xA2 : AND D
        Timing::Constant(1), // 0xA3 : AND E
        Timing::Constant(1), // 0xA4 : AND H
        Timing::Constant(1), // 0xA5 : AND L
        Timing::Constant(2), // 0xA6 : AND (HL)
        Timing::Constant(1), // 0xA7 : AND A
        Timing::Constant(1), // 0xA8 : XOR B
        Timing::Constant(1), // 0xA9 : XOR C
        Timing::Constant(1), // 0xAA : XOR D
        Timing::Constant(1), // 0xAB : XOR E
        Timing::Constant(1), // 0xAC : XOR H
        Timing::Constant(1), // 0xAD : XOR L
        Timing::Constant(2), // 0xAE : XOR (HL)
        Timing::Constant(1), // 0xAF : XOR A
        Timing::Constant(1), // 0xB0 : OR B
        Timing::Constant(1), // 0xB1 : OR C
        Timing::Constant(1), // 0xB2 : OR D
        Timing::Constant(1), // 0xB3 : OR E
        Timing::Constant(1), // 0xB4 : OR H
        Timing::Constant(1), // 0xB5 : OR L
        Timing::Constant(2), // 0xB6 : OR (HL)
        Timing::Constant(1), // 0xB7 : OR A
        Timing::Constant(1), // 0xB8 : CP B
        Timing::Constant(1), // 0xB9 : CP C
        Timing::Constant(1), // 0xBA : CP D
        Timing::Constant(1), // 0xBB : CP E
        Timing::Constant(1), // 0xBC : CP H
        Timing::Constant(1), // 0xBD : CP L
        Timing::Constant(2), // 0xBE : CP (HL)
        Timing::Constant(1), // 0xBF : CP A
        Timing::Branch(BranchTiming {
            timing_skipped: 2,
            timing_taken: 5,
            condition: BranchCondition::NonZero,
        }), // 0xC0 : RET NZ
        Timing::Constant(3), // 0xC1 : POP BC
        Timing::Branch(BranchTiming {
            timing_skipped: 3,
            timing_taken: 4,
            condition: BranchCondition::NonZero,
        }), // 0xC2 : JP NZ,a16
        Timing::Constant(4), // 0xC3 : JP a16
        Timing::Branch(BranchTiming {
            timing_skipped: 3,
            timing_taken: 6,
            condition: BranchCondition::NonZero,
        }), // 0xC4 : CALL NZ,a16
        Timing::Constant(4), // 0xC5 : PUSH BC
        Timing::Constant(2), // 0xC6 : ADD A,d8
        Timing::Constant(4), // 0xC7 : RST 00H
        Timing::Branch(BranchTiming {
            timing_skipped: 2,
            timing_taken: 5,
            condition: BranchCondition::Zero,
        }), // 0xC8 : RET Z
        Timing::Constant(4), // 0xC9 : RET
        Timing::Branch(BranchTiming {
            timing_skipped: 3,
            timing_taken: 4,
            condition: BranchCondition::Zero,
        }), // 0xCA : JP Z,a16
        Timing::Ignored,     // 0xCB : PREFIX CB
        Timing::Branch(BranchTiming {
            timing_skipped: 3,
            timing_taken: 6,
            condition: BranchCondition::Zero,
        }), // 0xCC : CALL Z,a16
        Timing::Constant(6), // 0xCD : CALL a16
        Timing::Constant(2), // 0xCE : ADC A,d8
        Timing::Constant(4), // 0xCF : RST 08H
        Timing::Branch(BranchTiming {
            timing_skipped: 2,
            timing_taken: 5,
            condition: BranchCondition::NonCarry,
        }), // 0xD0 : RET NC
        Timing::Constant(3), // 0xD1 : POP DE
        Timing::Branch(BranchTiming {
            timing_skipped: 3,
            timing_taken: 4,
            condition: BranchCondition::NonCarry,
        }), // 0xD2 : JP NC,a16
        Timing::Ignored,     // 0xD3 : undefined
        Timing::Branch(BranchTiming {
            timing_skipped: 3,
            timing_taken: 6,
            condition: BranchCondition::NonCarry,
        }), // 0xD4 : CALL NC,a16
        Timing::Constant(4), // 0xD5 : PUSH DE
        Timing::Constant(2), // 0xD6 : SUB d8
        Timing::Constant(4), // 0xD7 : RST 10H
        Timing::Branch(BranchTiming {
            timing_skipped: 2,
            timing_taken: 5,
            condition: BranchCondition::Carry,
        }), // 0xD8 : RET C
        Timing::Constant(4), // 0xD9 : RETI
        Timing::Branch(BranchTiming {
            timing_skipped: 3,
            timing_taken: 4,
            condition: BranchCondition::Carry,
        }), // 0xDA : JP C,a16
        Timing::Ignored,     // 0xDB : undefined
        Timing::Branch(BranchTiming {
            timing_skipped: 3,
            timing_taken: 6,
            condition: BranchCondition::Carry,
        }), // 0xDC : CALL C,a16
        Timing::Ignored,     // 0xDD : undefined
        Timing::Constant(2), // 0xDE : SBC A,d8
        Timing::Constant(4), // 0xDF : RST 18H
        Timing::Constant(3), // 0xE0 : LDH (a8),A
        Timing::Constant(3), // 0xE1 : POP HL
        Timing::Constant(2), // 0xE2 : LDH (C),A
        Timing::Ignored,     // 0xE3 : undefined
        Timing::Ignored,     // 0xE4 : undefined
        Timing::Constant(4), // 0xE5 : PUSH HL
        Timing::Constant(2), // 0xE6 : AND d8
        Timing::Constant(4), // 0xE7 : RST 20H
        Timing::Constant(4), // 0xE8 : ADD SP,r8
        Timing::Constant(1), // 0xE9 : JP (HL)
        Timing::Constant(4), // 0xEA : LD (a16),A
        Timing::Ignored,     // 0xEB : undefined
        Timing::Ignored,     // 0xEC : undefined
        Timing::Ignored,     // 0xED : undefined
        Timing::Constant(2), // 0xEE : XOR d8
        Timing::Constant(4), // 0xEF : RST 28H
        Timing::Constant(3), // 0xF0 : LDH A,(a8)
        Timing::Constant(3), // 0xF1 : POP AF
        Timing::Constant(2), // 0xF2 : LDH A,(C)
        Timing::Constant(1), // 0xF3 : DI
        Timing::Ignored,     // 0xF4 : undefined
        Timing::Constant(4), // 0xF5 : PUSH AF
        Timing::Constant(2), // 0xF6 : OR d8
        Timing::Constant(4), // 0xF7 : RST 30H
        Timing::Constant(3), // 0xF8 : LD HL,SP+r8
        Timing::Constant(2), // 0xF9 : LD SP,HL
        Timing::Constant(4), // 0xFA : LD A,(a16)
        Timing::Constant(1), // 0xFB : EI
        Timing::Ignored,     // 0xFC : undefined
        Timing::Ignored,     // 0xFD : undefined
        Timing::Constant(2), // 0xFE : CP d8
        Timing::Constant(4), // 0xFF : RST 38H
    ];

    struct AbsoluteCycleCounter {
        cycle_counter: u8,
    }

    impl Memory for AbsoluteCycleCounter {
        fn silent_read(&self, _: usize) -> u8 {
            self.cycle_counter
        }

        fn silent_write(&mut self, _: usize, _: u8) {}
    }

    impl InterruptEmitter for AbsoluteCycleCounter {
        fn tick(&mut self) -> Option<crate::emulator::interrupts::InterruptSource> {
            self.cycle_counter = self.cycle_counter.wrapping_add(1);
            None
        }
    }

    impl AbsoluteCycleCounter {
        pub fn new(offset: u8) -> Self {
            Self {
                cycle_counter: 0u8.wrapping_sub(offset),
            }
        }
    }

    fn new_cycle_counted_cpu(rom: &[u8], offset: u8) -> Cpu {
        Cpu::new(ConsoleMemory::new(
            rom,
            Some(Box::new(AbsoluteCycleCounter::new(offset))),
        ))
    }

    #[test]
    fn test_op_code_constant_timings() {
        OP_CODE_TIMINGS
            .into_iter()
            .enumerate()
            .filter_map(|(op_code, timing)| match timing {
                Timing::Constant(timing) => Some((op_code as u8, timing)),
                _ => None,
            })
            .for_each(|(op_code, expected_timing)| {
                let mut rom = vec![0; 0x0101];
                rom[0x100] = op_code;

                let mut cpu = new_cycle_counted_cpu(rom.as_slice(), 0);
                cpu.tick();

                let timing = cpu.memory.silent_read(timer::address::DIV);
                assert_eq!(
                    expected_timing, timing,
                    "Expected a constant time of {} for op code {:#04x}, got {} instead",
                    expected_timing, op_code, timing
                );
            });
    }

    #[test]
    fn test_op_code_branch_timings() {
        OP_CODE_TIMINGS
            .into_iter()
            .enumerate()
            .filter_map(|(op_code, timing)| match timing {
                Timing::Branch(timings) => Some((op_code as u8, timings)),
                _ => None,
            })
            .flat_map(|(op_code, timings)| {
                timings
                    .as_states_and_timings_iter()
                    .map(move |(status_flag, timing)| (op_code, status_flag, timing))
            })
            .for_each(|(op_code, status_flag, expected_timing)| {
                let mut rom = vec![0; 0xFFF5];
                rom[0x0100] = 0xF1;
                rom[0x0101] = op_code;
                rom[0xFFF4] = status_flag;

                let mut cpu = new_cycle_counted_cpu(rom.as_slice(), 3);
                cpu.tick();
                cpu.tick();

                let timing = cpu.memory.silent_read(timer::address::DIV);
                assert_eq!(
                    expected_timing, timing,
                    "Expected a time of {} for {:#04x} with flags {:#04x}, got {} instead",
                    expected_timing, op_code, status_flag, timing
                );
            });
    }
}
