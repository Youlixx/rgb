use crate::emulator::memory::MemoryMap;

use super::Cpu;

/// Gameboy SM63 CB prefixed opcodes implementations
impl<M: MemoryMap> Cpu<M> {
    /// Opcode 0x00: [RLC B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=81)
    ///
    /// Circular rotate left of the 8-bit register B (2 machine cycles).
    fn cb_rlc_b(&mut self) {
        let value = self.run_rlc_u8_and_update_flags(self.registers.b());
        self.registers.set_b(value);
    }

    /// Opcode 0x01: [RLC C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=81)
    ///
    /// Circular rotate left of the 8-bit register C (2 machine cycles).
    fn cb_rlc_c(&mut self) {
        let value = self.run_rlc_u8_and_update_flags(self.registers.c());
        self.registers.set_c(value);
    }

    /// Opcode 0x02: [RLC D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=81)
    ///
    /// Circular rotate left of the 8-bit register D (2 machine cycles).
    fn cb_rlc_d(&mut self) {
        let value = self.run_rlc_u8_and_update_flags(self.registers.d());
        self.registers.set_d(value);
    }

    /// Opcode 0x03: [RLC E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=81)
    ///
    /// Circular rotate left of the 8-bit register E (2 machine cycles).
    fn cb_rlc_e(&mut self) {
        let value = self.run_rlc_u8_and_update_flags(self.registers.e());
        self.registers.set_e(value);
    }

    /// Opcode 0x04: [RLC H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=81)
    ///
    /// Circular rotate left of the 8-bit register H (2 machine cycles).
    fn cb_rlc_h(&mut self) {
        let value = self.run_rlc_u8_and_update_flags(self.registers.h());
        self.registers.set_h(value);
    }

    /// Opcode 0x05: [RLC L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=81)
    ///
    /// Circular rotate left of the 8-bit register L (2 machine cycles).
    fn cb_rlc_l(&mut self) {
        let value = self.run_rlc_u8_and_update_flags(self.registers.l());
        self.registers.set_l(value);
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
        let value = self.run_rlc_u8_and_update_flags(self.registers.a());
        self.registers.set_a(value);
    }

    /// Opcode 0x08: [RRC B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=83)
    ///
    /// Circular rotate right of the 8-bit register B (2 machine cycles).
    fn cb_rrc_b(&mut self) {
        let value = self.run_rrc_u8_and_update_flags(self.registers.b());
        self.registers.set_b(value);
    }

    /// Opcode 0x09: [RRC C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=83)
    ///
    /// Circular rotate right of the 8-bit register C (2 machine cycles).
    fn cb_rrc_c(&mut self) {
        let value = self.run_rrc_u8_and_update_flags(self.registers.c());
        self.registers.set_c(value);
    }

    /// Opcode 0x0A: [RRC D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=83)
    ///
    /// Circular rotate right of the 8-bit register D (2 machine cycles).
    fn cb_rrc_d(&mut self) {
        let value = self.run_rrc_u8_and_update_flags(self.registers.d());
        self.registers.set_d(value);
    }

    /// Opcode 0x0B: [RRC E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=83)
    ///
    /// Circular rotate right of the 8-bit register E (2 machine cycles).
    fn cb_rrc_e(&mut self) {
        let value = self.run_rrc_u8_and_update_flags(self.registers.e());
        self.registers.set_e(value);
    }

    /// Opcode 0x0C: [RRC H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=83)
    ///
    /// Circular rotate right of the 8-bit register H (2 machine cycles).
    fn cb_rrc_h(&mut self) {
        let value = self.run_rrc_u8_and_update_flags(self.registers.h());
        self.registers.set_h(value);
    }

    /// Opcode 0x0D: [RRC L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=83)
    ///
    /// Circular rotate right of the 8-bit register L (2 machine cycles).
    fn cb_rrc_l(&mut self) {
        let value = self.run_rrc_u8_and_update_flags(self.registers.l());
        self.registers.set_l(value);
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
        let value = self.run_rrc_u8_and_update_flags(self.registers.a());
        self.registers.set_a(value);
    }

    /// Opcode 0x10: [RL B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=85)
    ///
    /// Rotate left of the 8-bit register B (2 machine cycles).
    fn cb_rl_b(&mut self) {
        let value = self.run_rl_u8_and_update_flags(self.registers.b());
        self.registers.set_b(value);
    }

    /// Opcode 0x11: [RL C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=85)
    ///
    /// Rotate left of the 8-bit register C (2 machine cycles).
    fn cb_rl_c(&mut self) {
        let value = self.run_rl_u8_and_update_flags(self.registers.c());
        self.registers.set_c(value);
    }

    /// Opcode 0x12: [RL D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=85)
    ///
    /// Rotate left of the 8-bit register D (2 machine cycles).
    fn cb_rl_d(&mut self) {
        let value = self.run_rl_u8_and_update_flags(self.registers.d());
        self.registers.set_d(value);
    }

    /// Opcode 0x13: [RL E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=85)
    ///
    /// Rotate left of the 8-bit register E (2 machine cycles).
    fn cb_rl_e(&mut self) {
        let value = self.run_rl_u8_and_update_flags(self.registers.e());
        self.registers.set_e(value);
    }

    /// Opcode 0x14: [RL H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=85)
    ///
    /// Rotate left of the 8-bit register H (2 machine cycles).
    fn cb_rl_h(&mut self) {
        let value = self.run_rl_u8_and_update_flags(self.registers.h());
        self.registers.set_h(value);
    }

    /// Opcode 0x15: [RL L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=85)
    ///
    /// Rotate left of the 8-bit register L (2 machine cycles).
    fn cb_rl_l(&mut self) {
        let value = self.run_rl_u8_and_update_flags(self.registers.l());
        self.registers.set_l(value);
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
        let value = self.run_rl_u8_and_update_flags(self.registers.a());
        self.registers.set_a(value);
    }

    /// Opcode 0x18: [RR B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=87)
    ///
    /// Rotate right of the 8-bit register B (2 machine cycles).
    fn cb_rr_b(&mut self) {
        let value = self.run_rr_u8_and_update_flags(self.registers.b());
        self.registers.set_b(value);
    }

    /// Opcode 0x19: [RR C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=87)
    ///
    /// Rotate right of the 8-bit register C (2 machine cycles).
    fn cb_rr_c(&mut self) {
        let value = self.run_rr_u8_and_update_flags(self.registers.c());
        self.registers.set_c(value);
    }

    /// Opcode 0x1A: [RR D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=87)
    ///
    /// Rotate right of the 8-bit register D (2 machine cycles).
    fn cb_rr_d(&mut self) {
        let value = self.run_rr_u8_and_update_flags(self.registers.d());
        self.registers.set_d(value);
    }

    /// Opcode 0x1B: [RR E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=87)
    ///
    /// Rotate right of the 8-bit register E (2 machine cycles).
    fn cb_rr_e(&mut self) {
        let value = self.run_rr_u8_and_update_flags(self.registers.e());
        self.registers.set_e(value);
    }

    /// Opcode 0x1C: [RR H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=87)
    ///
    /// Rotate right of the 8-bit register H (2 machine cycles).
    fn cb_rr_h(&mut self) {
        let value = self.run_rr_u8_and_update_flags(self.registers.h());
        self.registers.set_h(value);
    }

    /// Opcode 0x1D: [RR L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=87)
    ///
    /// Rotate right of the 8-bit register L (2 machine cycles).
    fn cb_rr_l(&mut self) {
        let value = self.run_rr_u8_and_update_flags(self.registers.l());
        self.registers.set_l(value);
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
        let value = self.run_rr_u8_and_update_flags(self.registers.a());
        self.registers.set_a(value);
    }

    /// Opcode 0x20: [SLA B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=89)
    ///
    /// Shift left of the 8-bit register B (2 machine cycles).
    fn cb_sla_b(&mut self) {
        let value = self.run_sla_u8_and_update_flags(self.registers.b());
        self.registers.set_b(value);
    }

    /// Opcode 0x21: [SLA C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=89)
    ///
    /// Shift left of the 8-bit register C (2 machine cycles).
    fn cb_sla_c(&mut self) {
        let value = self.run_sla_u8_and_update_flags(self.registers.c());
        self.registers.set_c(value);
    }

    /// Opcode 0x22: [SLA D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=89)
    ///
    /// Shift left of the 8-bit register D (2 machine cycles).
    fn cb_sla_d(&mut self) {
        let value = self.run_sla_u8_and_update_flags(self.registers.d());
        self.registers.set_d(value);
    }

    /// Opcode 0x23: [SLA E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=89)
    ///
    /// Shift left of the 8-bit register E (2 machine cycles).
    fn cb_sla_e(&mut self) {
        let value = self.run_sla_u8_and_update_flags(self.registers.e());
        self.registers.set_e(value);
    }

    /// Opcode 0x24: [SLA H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=89)
    ///
    /// Rotate left of the 8-bit register H (2 machine cycles).
    fn cb_sla_h(&mut self) {
        let value = self.run_sla_u8_and_update_flags(self.registers.h());
        self.registers.set_h(value);
    }

    /// Opcode 0x25: [SLA L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=89)
    ///
    /// Rotate left of the 8-bit register L (2 machine cycles).
    fn cb_sla_l(&mut self) {
        let value = self.run_sla_u8_and_update_flags(self.registers.l());
        self.registers.set_l(value);
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
        let value = self.run_sla_u8_and_update_flags(self.registers.a());
        self.registers.set_a(value);
    }

    /// Opcode 0x28: [RR B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=91)
    ///
    /// Shift right of the 8-bit register B (2 machine cycles).
    fn cb_sra_b(&mut self) {
        let value = self.run_sra_u8_and_update_flags(self.registers.b());
        self.registers.set_b(value);
    }

    /// Opcode 0x29: [RR C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=91)
    ///
    /// Shift right of the 8-bit register C (2 machine cycles).
    fn cb_sra_c(&mut self) {
        let value = self.run_sra_u8_and_update_flags(self.registers.c());
        self.registers.set_c(value);
    }

    /// Opcode 0x2A: [RR D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=91)
    ///
    /// Shift right of the 8-bit register D (2 machine cycles).
    fn cb_sra_d(&mut self) {
        let value = self.run_sra_u8_and_update_flags(self.registers.d());
        self.registers.set_d(value);
    }

    /// Opcode 0x2B: [RR E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=91)
    ///
    /// Shift right of the 8-bit register E (2 machine cycles).
    fn cb_sra_e(&mut self) {
        let value = self.run_sra_u8_and_update_flags(self.registers.e());
        self.registers.set_e(value);
    }

    /// Opcode 0x2C: [RR H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=91)
    ///
    /// Shift right of the 8-bit register H (2 machine cycles).
    fn cb_sra_h(&mut self) {
        let value = self.run_sra_u8_and_update_flags(self.registers.h());
        self.registers.set_h(value);
    }

    /// Opcode 0x2D: [RR L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=91)
    ///
    /// Shift right of the 8-bit register L (2 machine cycles).
    fn cb_sra_l(&mut self) {
        let value = self.run_sra_u8_and_update_flags(self.registers.l());
        self.registers.set_l(value);
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
        let value = self.run_sra_u8_and_update_flags(self.registers.a());
        self.registers.set_a(value);
    }

    /// Opcode 0x30: [SWAP B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=93)
    ///
    /// Swap the two halves of the 8-bit register B (2 machine cycles).
    fn cb_swap_b(&mut self) {
        let value = self.run_swap_u8_and_update_flags(self.registers.b());
        self.registers.set_b(value);
    }

    /// Opcode 0x31: [SWAP C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=93)
    ///
    /// Swap the two halves of the 8-bit register C (2 machine cycles).
    fn cb_swap_c(&mut self) {
        let value = self.run_swap_u8_and_update_flags(self.registers.c());
        self.registers.set_c(value);
    }

    /// Opcode 0x32: [SWAP D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=93)
    ///
    /// Swap the two halves of the 8-bit register D (2 machine cycles).
    fn cb_swap_d(&mut self) {
        let value = self.run_swap_u8_and_update_flags(self.registers.d());
        self.registers.set_d(value);
    }

    /// Opcode 0x33: [SWAP E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=93)
    ///
    /// Swap the two halves of the 8-bit register E (2 machine cycles).
    fn cb_swap_e(&mut self) {
        let value = self.run_swap_u8_and_update_flags(self.registers.e());
        self.registers.set_e(value);
    }

    /// Opcode 0x34: [SWAP H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=93)
    ///
    /// Swap the two halves of the 8-bit register H (2 machine cycles).
    fn cb_swap_h(&mut self) {
        let value = self.run_swap_u8_and_update_flags(self.registers.h());
        self.registers.set_h(value);
    }

    /// Opcode 0x35: [SWAP L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=93)
    ///
    /// Swap the two halves of the 8-bit register L (2 machine cycles).
    fn cb_swap_l(&mut self) {
        let value = self.run_swap_u8_and_update_flags(self.registers.l());
        self.registers.set_l(value);
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
        let value = self.run_swap_u8_and_update_flags(self.registers.a());
        self.registers.set_a(value);
    }

    /// Opcode 0x38: [SRL B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=95)
    ///
    /// Shift right of the 8-bit register B (2 machine cycles).
    fn cb_srl_b(&mut self) {
        let value = self.run_srl_u8_and_update_flags(self.registers.b());
        self.registers.set_b(value);
    }

    /// Opcode 0x39: [SRL C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=95)
    ///
    /// Shift right of the 8-bit register C (2 machine cycles).
    fn cb_srl_c(&mut self) {
        let value = self.run_srl_u8_and_update_flags(self.registers.c());
        self.registers.set_c(value);
    }

    /// Opcode 0x3A: [SRL D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=95)
    ///
    /// Shift right of the 8-bit register D (2 machine cycles).
    fn cb_srl_d(&mut self) {
        let value = self.run_srl_u8_and_update_flags(self.registers.d());
        self.registers.set_d(value);
    }

    /// Opcode 0x3B: [SRL E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=95)
    ///
    /// Shift right of the 8-bit register E (2 machine cycles).
    fn cb_srl_e(&mut self) {
        let value = self.run_srl_u8_and_update_flags(self.registers.e());
        self.registers.set_e(value);
    }

    /// Opcode 0x3C: [SRL H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=95)
    ///
    /// Shift right of the 8-bit register H (2 machine cycles).
    fn cb_srl_h(&mut self) {
        let value = self.run_srl_u8_and_update_flags(self.registers.h());
        self.registers.set_h(value);
    }

    /// Opcode 0x3D: [SRL L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=95)
    ///
    /// Shift right of the 8-bit register L (2 machine cycles).
    fn cb_srl_l(&mut self) {
        let value = self.run_srl_u8_and_update_flags(self.registers.l());
        self.registers.set_l(value);
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
        let value = self.run_srl_u8_and_update_flags(self.registers.a());
        self.registers.set_a(value);
    }

    /// Opcode 0x40: [BIT 0,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 0 of 8-bit register B is 0 (2 machine cycles).
    fn cb_bit_0_b(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.b(), 0x01);
    }

    /// Opcode 0x41: [BIT 0,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 0 of 8-bit register C is 0 (2 machine cycles).
    fn cb_bit_0_c(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.c(), 0x01);
    }

    /// Opcode 0x42: [BIT 0,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 0 of 8-bit register D is 0 (2 machine cycles).
    fn cb_bit_0_d(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.d(), 0x01);
    }

    /// Opcode 0x43: [BIT 0,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 0 of 8-bit register E is 0 (2 machine cycles).
    fn cb_bit_0_e(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.e(), 0x01);
    }

    /// Opcode 0x44: [BIT 0,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 0 of 8-bit register H is 0 (2 machine cycles).
    fn cb_bit_0_h(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.h(), 0x01);
    }

    /// Opcode 0x45: [BIT 0,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 0 of 8-bit register L is 0 (2 machine cycles).
    fn cb_bit_0_l(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.l(), 0x01);
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
        self.run_bit_u8_and_update_flags(self.registers.a(), 0x01);
    }

    /// Opcode 0x48: [BIT 1,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 1 of 8-bit register B is 0 (2 machine cycles).
    fn cb_bit_1_b(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.b(), 0x02);
    }

    /// Opcode 0x49: [BIT 1,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 1 of 8-bit register C is 0 (2 machine cycles).
    fn cb_bit_1_c(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.c(), 0x02);
    }

    /// Opcode 0x4A: [BIT 1,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 1 of 8-bit register D is 0 (2 machine cycles).
    fn cb_bit_1_d(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.d(), 0x02);
    }

    /// Opcode 0x4B: [BIT 1,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 1 of 8-bit register E is 0 (2 machine cycles).
    fn cb_bit_1_e(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.e(), 0x02);
    }

    /// Opcode 0x4C: [BIT 1,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 1 of 8-bit register H is 0 (2 machine cycles).
    fn cb_bit_1_h(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.h(), 0x02);
    }

    /// Opcode 0x4D: [BIT 1,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 1 of 8-bit register L is 0 (2 machine cycles).
    fn cb_bit_1_l(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.l(), 0x02);
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
        self.run_bit_u8_and_update_flags(self.registers.a(), 0x02);
    }

    /// Opcode 0x50: [BIT 2,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 2 of 8-bit register B is 0 (2 machine cycles).
    fn cb_bit_2_b(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.b(), 0x04);
    }

    /// Opcode 0x51: [BIT 2,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 2 of 8-bit register C is 0 (2 machine cycles).
    fn cb_bit_2_c(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.c(), 0x04);
    }

    /// Opcode 0x52: [BIT 2,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 2 of 8-bit register D is 0 (2 machine cycles).
    fn cb_bit_2_d(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.d(), 0x04);
    }

    /// Opcode 0x53: [BIT 2,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 2 of 8-bit register E is 0 (2 machine cycles).
    fn cb_bit_2_e(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.e(), 0x04);
    }

    /// Opcode 0x54: [BIT 2,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 2 of 8-bit register H is 0 (2 machine cycles).
    fn cb_bit_2_h(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.h(), 0x04);
    }

    /// Opcode 0x55: [BIT 2,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 2 of 8-bit register L is 0 (2 machine cycles).
    fn cb_bit_2_l(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.l(), 0x04);
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
        self.run_bit_u8_and_update_flags(self.registers.a(), 0x04);
    }

    /// Opcode 0x58: [BIT 3,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 3 of 8-bit register B is 0 (2 machine cycles).
    fn cb_bit_3_b(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.b(), 0x08);
    }

    /// Opcode 0x59: [BIT 3,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 3 of 8-bit register C is 0 (2 machine cycles).
    fn cb_bit_3_c(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.c(), 0x08);
    }

    /// Opcode 0x5A: [BIT 3,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 3 of 8-bit register D is 0 (2 machine cycles).
    fn cb_bit_3_d(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.d(), 0x08);
    }

    /// Opcode 0x5B: [BIT 3,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 3 of 8-bit register E is 0 (2 machine cycles).
    fn cb_bit_3_e(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.e(), 0x08);
    }

    /// Opcode 0x5C: [BIT 3,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 3 of 8-bit register H is 0 (2 machine cycles).
    fn cb_bit_3_h(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.h(), 0x08);
    }

    /// Opcode 0x5D: [BIT 3,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 3 of 8-bit register L is 0 (2 machine cycles).
    fn cb_bit_3_l(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.l(), 0x08);
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
        self.run_bit_u8_and_update_flags(self.registers.a(), 0x08);
    }

    /// Opcode 0x60: [BIT 4,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 4 of 8-bit register B is 0 (2 machine cycles).
    fn cb_bit_4_b(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.b(), 0x10);
    }

    /// Opcode 0x61: [BIT 4,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 4 of 8-bit register C is 0 (2 machine cycles).
    fn cb_bit_4_c(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.c(), 0x10);
    }

    /// Opcode 0x62: [BIT 4,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 4 of 8-bit register D is 0 (2 machine cycles).
    fn cb_bit_4_d(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.d(), 0x10);
    }

    /// Opcode 0x63: [BIT 4,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 4 of 8-bit register E is 0 (2 machine cycles).
    fn cb_bit_4_e(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.e(), 0x10);
    }

    /// Opcode 0x64: [BIT 4,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 4 of 8-bit register H is 0 (2 machine cycles).
    fn cb_bit_4_h(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.h(), 0x10);
    }

    /// Opcode 0x65: [BIT 4,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 4 of 8-bit register L is 0 (2 machine cycles).
    fn cb_bit_4_l(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.l(), 0x10);
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
        self.run_bit_u8_and_update_flags(self.registers.a(), 0x10);
    }

    /// Opcode 0x68: [BIT 5,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 5 of 8-bit register B is 0 (2 machine cycles).
    fn cb_bit_5_b(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.b(), 0x20);
    }

    /// Opcode 0x69: [BIT 5,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 5 of 8-bit register C is 0 (2 machine cycles).
    fn cb_bit_5_c(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.c(), 0x20);
    }

    /// Opcode 0x6A: [BIT 5,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 5 of 8-bit register D is 0 (2 machine cycles).
    fn cb_bit_5_d(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.d(), 0x20);
    }

    /// Opcode 0x6B: [BIT 5,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 5 of 8-bit register E is 0 (2 machine cycles).
    fn cb_bit_5_e(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.e(), 0x20);
    }

    /// Opcode 0x6C: [BIT 5,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 5 of 8-bit register H is 0 (2 machine cycles).
    fn cb_bit_5_h(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.h(), 0x20);
    }

    /// Opcode 0x6D: [BIT 5,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 5 of 8-bit register L is 0 (2 machine cycles).
    fn cb_bit_5_l(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.l(), 0x20);
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
        self.run_bit_u8_and_update_flags(self.registers.a(), 0x20);
    }

    /// Opcode 0x70: [BIT 6,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 6 of 8-bit register B is 0 (2 machine cycles).
    fn cb_bit_6_b(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.b(), 0x40);
    }

    /// Opcode 0x71: [BIT 6,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 6 of 8-bit register C is 0 (2 machine cycles).
    fn cb_bit_6_c(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.c(), 0x40);
    }

    /// Opcode 0x72: [BIT 6,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 6 of 8-bit register D is 0 (2 machine cycles).
    fn cb_bit_6_d(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.d(), 0x40);
    }

    /// Opcode 0x73: [BIT 6,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 6 of 8-bit register E is 0 (2 machine cycles).
    fn cb_bit_6_e(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.e(), 0x40);
    }

    /// Opcode 0x74: [BIT 6,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 6 of 8-bit register H is 0 (2 machine cycles).
    fn cb_bit_6_h(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.h(), 0x40);
    }

    /// Opcode 0x75: [BIT 6,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 6 of 8-bit register L is 0 (2 machine cycles).
    fn cb_bit_6_l(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.l(), 0x40);
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
        self.run_bit_u8_and_update_flags(self.registers.a(), 0x40);
    }

    /// Opcode 0x78: [BIT 7,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 7 of 8-bit register B is 0 (2 machine cycles).
    fn cb_bit_7_b(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.b(), 0x80);
    }

    /// Opcode 0x79: [BIT 7,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 7 of 8-bit register C is 0 (2 machine cycles).
    fn cb_bit_7_c(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.c(), 0x80);
    }

    /// Opcode 0x7A: [BIT 7,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 7 of 8-bit register D is 0 (2 machine cycles).
    fn cb_bit_7_d(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.d(), 0x80);
    }

    /// Opcode 0x7B: [BIT 7,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 7 of 8-bit register E is 0 (2 machine cycles).
    fn cb_bit_7_e(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.e(), 0x80);
    }

    /// Opcode 0x7C: [BIT 7,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 7 of 8-bit register H is 0 (2 machine cycles).
    fn cb_bit_7_h(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.h(), 0x80);
    }

    /// Opcode 0x7D: [BIT 7,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=97)
    ///
    /// Test if the bit 7 of 8-bit register L is 0 (2 machine cycles).
    fn cb_bit_7_l(&mut self) {
        self.run_bit_u8_and_update_flags(self.registers.l(), 0x80);
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
        self.run_bit_u8_and_update_flags(self.registers.a(), 0x80);
    }

    /// Opcode 0x80: [RES 0,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 0 of 8-bit register B (2 machine cycles).
    fn cb_res_0_b(&mut self) {
        self.registers.set_b(self.registers.b() & 0xFE);
    }

    /// Opcode 0x81: [RES 0,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 0 of 8-bit register C (2 machine cycles).
    fn cb_res_0_c(&mut self) {
        self.registers.set_c(self.registers.c() & 0xFE);
    }

    /// Opcode 0x82: [RES 0,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 0 of 8-bit register D (2 machine cycles).
    fn cb_res_0_d(&mut self) {
        self.registers.set_d(self.registers.d() & 0xFE);
    }

    /// Opcode 0x83: [RES 0,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 0 of 8-bit register E (2 machine cycles).
    fn cb_res_0_e(&mut self) {
        self.registers.set_e(self.registers.e() & 0xFE);
    }

    /// Opcode 0x84: [RES 0,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 0 of 8-bit register H (2 machine cycles).
    fn cb_res_0_h(&mut self) {
        self.registers.set_h(self.registers.h() & 0xFE);
    }

    /// Opcode 0x85: [RES 0,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 0 of 8-bit register L (2 machine cycles).
    fn cb_res_0_l(&mut self) {
        self.registers.set_l(self.registers.l() & 0xFE);
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
        self.registers.set_a(self.registers.a() & 0xFE);
    }

    /// Opcode 0x88: [RES 1,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 1 of 8-bit register B (2 machine cycles).
    fn cb_res_1_b(&mut self) {
        self.registers.set_b(self.registers.b() & 0xFD);
    }

    /// Opcode 0x89: [RES 1,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 1 of 8-bit register C (2 machine cycles).
    fn cb_res_1_c(&mut self) {
        self.registers.set_c(self.registers.c() & 0xFD);
    }

    /// Opcode 0x8A: [RES 1,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 1 of 8-bit register D (2 machine cycles).
    fn cb_res_1_d(&mut self) {
        self.registers.set_d(self.registers.d() & 0xFD);
    }

    /// Opcode 0x8B: [RES 1,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 1 of 8-bit register E (2 machine cycles).
    fn cb_res_1_e(&mut self) {
        self.registers.set_e(self.registers.e() & 0xFD);
    }

    /// Opcode 0x8C: [RES 1,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 1 of 8-bit register H (2 machine cycles).
    fn cb_res_1_h(&mut self) {
        self.registers.set_h(self.registers.h() & 0xFD);
    }

    /// Opcode 0x8D: [RES 1,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 1 of 8-bit register L (2 machine cycles).
    fn cb_res_1_l(&mut self) {
        self.registers.set_l(self.registers.l() & 0xFD);
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
        self.registers.set_a(self.registers.a() & 0xFD);
    }

    /// Opcode 0x90: [RES 2,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 2 of 8-bit register B (2 machine cycles).
    fn cb_res_2_b(&mut self) {
        self.registers.set_b(self.registers.b() & 0xFB);
    }

    /// Opcode 0x91: [RES 2,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 2 of 8-bit register C (2 machine cycles).
    fn cb_res_2_c(&mut self) {
        self.registers.set_c(self.registers.c() & 0xFB);
    }

    /// Opcode 0x92: [RES 2,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 2 of 8-bit register D (2 machine cycles).
    fn cb_res_2_d(&mut self) {
        self.registers.set_d(self.registers.d() & 0xFB);
    }

    /// Opcode 0x93: [RES 2,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 2 of 8-bit register E (2 machine cycles).
    fn cb_res_2_e(&mut self) {
        self.registers.set_e(self.registers.e() & 0xFB);
    }

    /// Opcode 0x94: [RES 2,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 2 of 8-bit register H (2 machine cycles).
    fn cb_res_2_h(&mut self) {
        self.registers.set_h(self.registers.h() & 0xFB);
    }

    /// Opcode 0x95: [RES 2,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 2 of 8-bit register L (2 machine cycles).
    fn cb_res_2_l(&mut self) {
        self.registers.set_l(self.registers.l() & 0xFB);
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
        self.registers.set_a(self.registers.a() & 0xFB);
    }

    /// Opcode 0x98: [RES 3,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 3 of 8-bit register B (2 machine cycles).
    fn cb_res_3_b(&mut self) {
        self.registers.set_b(self.registers.b() & 0xF7);
    }

    /// Opcode 0x99: [RES 3,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 3 of 8-bit register C (2 machine cycles).
    fn cb_res_3_c(&mut self) {
        self.registers.set_c(self.registers.c() & 0xF7);
    }

    /// Opcode 0x9A: [RES 3,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 3 of 8-bit register D (2 machine cycles).
    fn cb_res_3_d(&mut self) {
        self.registers.set_d(self.registers.d() & 0xF7);
    }

    /// Opcode 0x9B: [RES 3,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 3 of 8-bit register E (2 machine cycles).
    fn cb_res_3_e(&mut self) {
        self.registers.set_e(self.registers.e() & 0xF7);
    }

    /// Opcode 0x9C: [RES 3,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 3 of 8-bit register H (2 machine cycles).
    fn cb_res_3_h(&mut self) {
        self.registers.set_h(self.registers.h() & 0xF7);
    }

    /// Opcode 0x9D: [RES 3,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 3 of 8-bit register L (2 machine cycles).
    fn cb_res_3_l(&mut self) {
        self.registers.set_l(self.registers.l() & 0xF7);
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
        self.registers.set_a(self.registers.a() & 0xF7);
    }

    /// Opcode 0xA0: [RES 4,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 4 of 8-bit register B (2 machine cycles).
    fn cb_res_4_b(&mut self) {
        self.registers.set_b(self.registers.b() & 0xEF);
    }

    /// Opcode 0xA1: [RES 4,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 4 of 8-bit register C (2 machine cycles).
    fn cb_res_4_c(&mut self) {
        self.registers.set_c(self.registers.c() & 0xEF);
    }

    /// Opcode 0xA2: [RES 4,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 4 of 8-bit register D (2 machine cycles).
    fn cb_res_4_d(&mut self) {
        self.registers.set_d(self.registers.d() & 0xEF);
    }

    /// Opcode 0xA3: [RES 4,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 4 of 8-bit register E (2 machine cycles).
    fn cb_res_4_e(&mut self) {
        self.registers.set_e(self.registers.e() & 0xEF);
    }

    /// Opcode 0xA4: [RES 4,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 4 of 8-bit register H (2 machine cycles).
    fn cb_res_4_h(&mut self) {
        self.registers.set_h(self.registers.h() & 0xEF);
    }

    /// Opcode 0xA5: [RES 4,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 4 of 8-bit register L (2 machine cycles).
    fn cb_res_4_l(&mut self) {
        self.registers.set_l(self.registers.l() & 0xEF);
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
        self.registers.set_a(self.registers.a() & 0xEF);
    }

    /// Opcode 0xA8: [RES 5,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 5 of 8-bit register B (2 machine cycles).
    fn cb_res_5_b(&mut self) {
        self.registers.set_b(self.registers.b() & 0xDF);
    }

    /// Opcode 0xA9: [RES 5,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 5 of 8-bit register C (2 machine cycles).
    fn cb_res_5_c(&mut self) {
        self.registers.set_c(self.registers.c() & 0xDF);
    }

    /// Opcode 0xAA: [RES 5,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 5 of 8-bit register D (2 machine cycles).
    fn cb_res_5_d(&mut self) {
        self.registers.set_d(self.registers.d() & 0xDF);
    }

    /// Opcode 0xAB: [RES 5,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 5 of 8-bit register E (2 machine cycles).
    fn cb_res_5_e(&mut self) {
        self.registers.set_e(self.registers.e() & 0xDF);
    }

    /// Opcode 0xAC: [RES 5,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 5 of 8-bit register H (2 machine cycles).
    fn cb_res_5_h(&mut self) {
        self.registers.set_h(self.registers.h() & 0xDF);
    }

    /// Opcode 0xAD: [RES 5,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 5 of 8-bit register L (2 machine cycles).
    fn cb_res_5_l(&mut self) {
        self.registers.set_l(self.registers.l() & 0xDF);
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
        self.registers.set_a(self.registers.a() & 0xDF);
    }

    /// Opcode 0xB0: [RES 6,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 6 of 8-bit register B (2 machine cycles).
    fn cb_res_6_b(&mut self) {
        self.registers.set_b(self.registers.b() & 0xBF);
    }

    /// Opcode 0xB1: [RES 6,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 6 of 8-bit register C (2 machine cycles).
    fn cb_res_6_c(&mut self) {
        self.registers.set_c(self.registers.c() & 0xBF);
    }

    /// Opcode 0xB2: [RES 6,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 6 of 8-bit register D (2 machine cycles).
    fn cb_res_6_d(&mut self) {
        self.registers.set_d(self.registers.d() & 0xBF);
    }

    /// Opcode 0xB3: [RES 6,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 6 of 8-bit register E (2 machine cycles).
    fn cb_res_6_e(&mut self) {
        self.registers.set_e(self.registers.e() & 0xBF);
    }

    /// Opcode 0xB4: [RES 6,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 6 of 8-bit register H (2 machine cycles).
    fn cb_res_6_h(&mut self) {
        self.registers.set_h(self.registers.h() & 0xBF);
    }

    /// Opcode 0xB5: [RES 6,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 6 of 8-bit register L (2 machine cycles).
    fn cb_res_6_l(&mut self) {
        self.registers.set_l(self.registers.l() & 0xBF);
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
        self.registers.set_a(self.registers.a() & 0xBF);
    }

    /// Opcode 0xB8: [RES 7,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 7 of 8-bit register B (2 machine cycles).
    fn cb_res_7_b(&mut self) {
        self.registers.set_b(self.registers.b() & 0x7F);
    }

    /// Opcode 0xB9: [RES 7,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 7 of 8-bit register C (2 machine cycles).
    fn cb_res_7_c(&mut self) {
        self.registers.set_c(self.registers.c() & 0x7F);
    }

    /// Opcode 0xBA: [RES 7,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 7 of 8-bit register D (2 machine cycles).
    fn cb_res_7_d(&mut self) {
        self.registers.set_d(self.registers.d() & 0x7F);
    }

    /// Opcode 0xBB: [RES 7,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 7 of 8-bit register E (2 machine cycles).
    fn cb_res_7_e(&mut self) {
        self.registers.set_e(self.registers.e() & 0x7F);
    }

    /// Opcode 0xBC: [RES 7,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 7 of 8-bit register H (2 machine cycles).
    fn cb_res_7_h(&mut self) {
        self.registers.set_h(self.registers.h() & 0x7F);
    }

    /// Opcode 0xBD: [RES 7,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=99)
    ///
    /// Reset bit 7 of 8-bit register L (2 machine cycles).
    fn cb_res_7_l(&mut self) {
        self.registers.set_l(self.registers.l() & 0x7F);
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
        self.registers.set_a(self.registers.a() & 0x7F);
    }

    /// Opcode 0xC0: [SET 0,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 0 of 8-bit register B (2 machine cycles).
    fn cb_set_0_b(&mut self) {
        self.registers.set_b(self.registers.b() | 0x01);
    }

    /// Opcode 0xC1: [SET 0,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 0 of 8-bit register C (2 machine cycles).
    fn cb_set_0_c(&mut self) {
        self.registers.set_c(self.registers.c() | 0x01);
    }

    /// Opcode 0xC2: [SET 0,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 0 of 8-bit register D (2 machine cycles).
    fn cb_set_0_d(&mut self) {
        self.registers.set_d(self.registers.d() | 0x01);
    }

    /// Opcode 0xC3: [SET 0,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 0 of 8-bit register E (2 machine cycles).
    fn cb_set_0_e(&mut self) {
        self.registers.set_e(self.registers.e() | 0x01);
    }

    /// Opcode 0xC4: [SET 0,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 0 of 8-bit register H (2 machine cycles).
    fn cb_set_0_h(&mut self) {
        self.registers.set_h(self.registers.h() | 0x01);
    }

    /// Opcode 0xC5: [SET 0,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 0 of 8-bit register L (2 machine cycles).
    fn cb_set_0_l(&mut self) {
        self.registers.set_l(self.registers.l() | 0x01);
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
        self.registers.set_a(self.registers.a() | 0x01);
    }

    /// Opcode 0xC8: [SET 1,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 1 of 8-bit register B (2 machine cycles).
    fn cb_set_1_b(&mut self) {
        self.registers.set_b(self.registers.b() | 0x02);
    }

    /// Opcode 0xC9: [SET 1,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 1 of 8-bit register C (2 machine cycles).
    fn cb_set_1_c(&mut self) {
        self.registers.set_c(self.registers.c() | 0x02);
    }

    /// Opcode 0xCA: [SET 1,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 1 of 8-bit register D (2 machine cycles).
    fn cb_set_1_d(&mut self) {
        self.registers.set_d(self.registers.d() | 0x02);
    }

    /// Opcode 0xCB: [SET 1,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 1 of 8-bit register E (2 machine cycles).
    fn cb_set_1_e(&mut self) {
        self.registers.set_e(self.registers.e() | 0x02);
    }

    /// Opcode 0xCC: [SET 1,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 1 of 8-bit register H (2 machine cycles).
    fn cb_set_1_h(&mut self) {
        self.registers.set_h(self.registers.h() | 0x02);
    }

    /// Opcode 0xCD: [SET 1,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 1 of 8-bit register L (2 machine cycles).
    fn cb_set_1_l(&mut self) {
        self.registers.set_l(self.registers.l() | 0x02);
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
        self.registers.set_a(self.registers.a() | 0x02);
    }

    /// Opcode 0xD0: [SET 2,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 2 of 8-bit register B (2 machine cycles).
    fn cb_set_2_b(&mut self) {
        self.registers.set_b(self.registers.b() | 0x04);
    }

    /// Opcode 0xD1: [SET 2,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 2 of 8-bit register C (2 machine cycles).
    fn cb_set_2_c(&mut self) {
        self.registers.set_c(self.registers.c() | 0x04);
    }

    /// Opcode 0xD2: [SET 2,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 2 of 8-bit register D (2 machine cycles).
    fn cb_set_2_d(&mut self) {
        self.registers.set_d(self.registers.d() | 0x04);
    }

    /// Opcode 0xD3: [SET 2,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 2 of 8-bit register E (2 machine cycles).
    fn cb_set_2_e(&mut self) {
        self.registers.set_e(self.registers.e() | 0x04);
    }

    /// Opcode 0xD4: [SET 2,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 2 of 8-bit register H (2 machine cycles).
    fn cb_set_2_h(&mut self) {
        self.registers.set_h(self.registers.h() | 0x04);
    }

    /// Opcode 0xD5: [SET 2,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 2 of 8-bit register L (2 machine cycles).
    fn cb_set_2_l(&mut self) {
        self.registers.set_l(self.registers.l() | 0x04);
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
        self.registers.set_a(self.registers.a() | 0x04);
    }

    /// Opcode 0xD8: [SET 3,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 3 of 8-bit register B (2 machine cycles).
    fn cb_set_3_b(&mut self) {
        self.registers.set_b(self.registers.b() | 0x08);
    }

    /// Opcode 0xD9: [SET 3,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 3 of 8-bit register C (2 machine cycles).
    fn cb_set_3_c(&mut self) {
        self.registers.set_c(self.registers.c() | 0x08);
    }

    /// Opcode 0xDA: [SET 3,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 3 of 8-bit register D (2 machine cycles).
    fn cb_set_3_d(&mut self) {
        self.registers.set_d(self.registers.d() | 0x08);
    }

    /// Opcode 0xDB: [SET 3,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 3 of 8-bit register E (2 machine cycles).
    fn cb_set_3_e(&mut self) {
        self.registers.set_e(self.registers.e() | 0x08);
    }

    /// Opcode 0xDC: [SET 3,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 3 of 8-bit register H (2 machine cycles).
    fn cb_set_3_h(&mut self) {
        self.registers.set_h(self.registers.h() | 0x08);
    }

    /// Opcode 0xDD: [SET 3,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 3 of 8-bit register L (2 machine cycles).
    fn cb_set_3_l(&mut self) {
        self.registers.set_l(self.registers.l() | 0x08);
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
        self.registers.set_a(self.registers.a() | 0x08);
    }

    /// Opcode 0xE0: [SET 4,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 4 of 8-bit register B (2 machine cycles).
    fn cb_set_4_b(&mut self) {
        self.registers.set_b(self.registers.b() | 0x10);
    }

    /// Opcode 0xE1: [SET 4,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 4 of 8-bit register C (2 machine cycles).
    fn cb_set_4_c(&mut self) {
        self.registers.set_c(self.registers.c() | 0x10);
    }

    /// Opcode 0xE2: [SET 4,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 4 of 8-bit register D (2 machine cycles).
    fn cb_set_4_d(&mut self) {
        self.registers.set_d(self.registers.d() | 0x10);
    }

    /// Opcode 0xE3: [SET 4,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 4 of 8-bit register E (2 machine cycles).
    fn cb_set_4_e(&mut self) {
        self.registers.set_e(self.registers.e() | 0x10);
    }

    /// Opcode 0xE4: [SET 4,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 4 of 8-bit register H (2 machine cycles).
    fn cb_set_4_h(&mut self) {
        self.registers.set_h(self.registers.h() | 0x10);
    }

    /// Opcode 0xE5: [SET 4,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 4 of 8-bit register L (2 machine cycles).
    fn cb_set_4_l(&mut self) {
        self.registers.set_l(self.registers.l() | 0x10);
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
        self.registers.set_a(self.registers.a() | 0x10);
    }

    /// Opcode 0xE8: [SET 5,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 5 of 8-bit register B (2 machine cycles).
    fn cb_set_5_b(&mut self) {
        self.registers.set_b(self.registers.b() | 0x20);
    }

    /// Opcode 0xE9: [SET 5,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 5 of 8-bit register C (2 machine cycles).
    fn cb_set_5_c(&mut self) {
        self.registers.set_c(self.registers.c() | 0x20);
    }

    /// Opcode 0xEA: [SET 5,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 5 of 8-bit register D (2 machine cycles).
    fn cb_set_5_d(&mut self) {
        self.registers.set_d(self.registers.d() | 0x20);
    }

    /// Opcode 0xEB: [SET 5,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 5 of 8-bit register E (2 machine cycles).
    fn cb_set_5_e(&mut self) {
        self.registers.set_e(self.registers.e() | 0x20);
    }

    /// Opcode 0xEC: [SET 5,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 5 of 8-bit register H (2 machine cycles).
    fn cb_set_5_h(&mut self) {
        self.registers.set_h(self.registers.h() | 0x20);
    }

    /// Opcode 0xED: [SET 5,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 5 of 8-bit register L (2 machine cycles).
    fn cb_set_5_l(&mut self) {
        self.registers.set_l(self.registers.l() | 0x20);
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
        self.registers.set_a(self.registers.a() | 0x20);
    }

    /// Opcode 0xF0: [SET 6,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 6 of 8-bit register B (2 machine cycles).
    fn cb_set_6_b(&mut self) {
        self.registers.set_b(self.registers.b() | 0x40);
    }

    /// Opcode 0xF1: [SET 6,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 6 of 8-bit register C (2 machine cycles).
    fn cb_set_6_c(&mut self) {
        self.registers.set_c(self.registers.c() | 0x40);
    }

    /// Opcode 0xF2: [SET 6,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 6 of 8-bit register D (2 machine cycles).
    fn cb_set_6_d(&mut self) {
        self.registers.set_d(self.registers.d() | 0x40);
    }

    /// Opcode 0xF3: [SET 6,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 6 of 8-bit register E (2 machine cycles).
    fn cb_set_6_e(&mut self) {
        self.registers.set_e(self.registers.e() | 0x40);
    }

    /// Opcode 0xF4: [SET 6,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 6 of 8-bit register H (2 machine cycles).
    fn cb_set_6_h(&mut self) {
        self.registers.set_h(self.registers.h() | 0x40);
    }

    /// Opcode 0xF5: [SET 6,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 6 of 8-bit register L (2 machine cycles).
    fn cb_set_6_l(&mut self) {
        self.registers.set_l(self.registers.l() | 0x40);
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
        self.registers.set_a(self.registers.a() | 0x40);
    }

    /// Opcode 0xF8: [SET 7,B](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 7 of 8-bit register B (2 machine cycles).
    fn cb_set_7_b(&mut self) {
        self.registers.set_b(self.registers.b() | 0x80);
    }

    /// Opcode 0xF9: [SET 7,C](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 7 of 8-bit register C (2 machine cycles).
    fn cb_set_7_c(&mut self) {
        self.registers.set_c(self.registers.c() | 0x80);
    }

    /// Opcode 0xFA: [SET 7,D](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 7 of 8-bit register D (2 machine cycles).
    fn cb_set_7_d(&mut self) {
        self.registers.set_d(self.registers.d() | 0x80);
    }

    /// Opcode 0xFB: [SET 7,E](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 7 of 8-bit register E (2 machine cycles).
    fn cb_set_7_e(&mut self) {
        self.registers.set_e(self.registers.e() | 0x80);
    }

    /// Opcode 0xFC: [SET 7,H](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 7 of 8-bit register H (2 machine cycles).
    fn cb_set_7_h(&mut self) {
        self.registers.set_h(self.registers.h() | 0x80);
    }

    /// Opcode 0xFD: [SET 7,L](https://gekkio.fi/files/gb-docs/gbctr.pdf#page=101)
    ///
    /// Set bit 7 of 8-bit register L (2 machine cycles).
    fn cb_set_7_l(&mut self) {
        self.registers.set_l(self.registers.l() | 0x80);
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
        self.registers.set_a(self.registers.a() | 0x80);
    }

    /// CB code function pointers.
    pub const CB_CODE_FUNCTION_TABLE: [fn(&mut Cpu<M>); 256] = [
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
}

#[cfg(test)]
mod tests {
    use crate::emulator::{cpu::tests::new_cycle_counted_cpu, memory::MemoryMap};

    const CB_CODE_TIMINGS: [u8; 256] = [
        2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, // 0x0
        2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, // 0x1
        2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, // 0x2
        2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, // 0x3
        2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2, // 0x4
        2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2, // 0x5
        2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2, // 0x6
        2, 2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 3, 2, // 0x7
        2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, // 0x8
        2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, // 0x9
        2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, // 0xA
        2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, // 0xB
        2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, // 0xC
        2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, // 0xD
        2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, // 0xE
        2, 2, 2, 2, 2, 2, 4, 2, 2, 2, 2, 2, 2, 2, 4, 2, // 0xF
    ];

    #[test]
    fn test_cb_code_timings() {
        CB_CODE_TIMINGS
            .into_iter()
            .enumerate()
            .for_each(|(cb_code, expected_timing)| {
                let mut rom = vec![0; 0x0102];
                rom[0x100] = 0xCB;
                rom[0x101] = cb_code as u8;

                let mut cpu = new_cycle_counted_cpu(rom.as_slice(), 0);
                cpu.tick();

                let timing = cpu.memory.read(0x00FF);
                assert_eq!(
                    expected_timing, timing,
                    "Expected a constant time of {} for CB code {:#04x}, got {} instead",
                    expected_timing, cb_code, timing
                );
            });
    }
}
