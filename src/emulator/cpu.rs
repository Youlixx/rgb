mod cb_codes;
mod op_codes;
mod register;

use self::{op_codes::OP_CODE_FUNCTION_TABLE, register::CpuRegisters};

use super::{memory::ConsoleMemory, timer::Timer};

mod status_flag {
    pub const ZERO: u8 = 0x80;
    pub const NEGATIVE: u8 = 0x40;
    pub const HALF_CARRY: u8 = 0x20;
    pub const CARRY: u8 = 0x10;
}

// struct CpuRegisters {
//     register_a: u8,
//     register_b: u8,
//     register_c: u8,
//     register_d: u8,
//     register_e: u8,
//     register_h: u8,
//     register_l: u8,
// }

// impl CpuRegisters {
//     fn new() -> Self {
//         Self {
//             register_a: 0,
//             register_b: 0,
//             register_c: 0,
//             register_d: 0,
//             register_e: 0,
//             register_h: 0,
//             register_l: 0,
//         }
//     }

//     fn bc(&self) -> u16 {
//         ((self.register_b as u16) << 8) | self.register_c as u16
//     }

//     fn set_bc(&mut self, value: u16) {
//         self.register_b = (value >> 8) as u8;
//         self.register_c = (value & 0xFF) as u8;
//     }

//     fn de(&self) -> u16 {
//         ((self.register_d as u16) << 8) | self.register_e as u16
//     }

//     fn set_de(&mut self, value: u16) {
//         self.register_d = (value >> 8) as u8;
//         self.register_e = (value & 0xFF) as u8;
//     }

//     fn hl(&self) -> u16 {
//         ((self.register_h as u16) << 8) | self.register_l as u16
//     }

//     fn set_hl(&mut self, value: u16) {
//         self.register_h = (value >> 8) as u8;
//         self.register_l = (value & 0xFF) as u8;
//     }
// }

pub struct Cpu {
    memory: ConsoleMemory,
    program_counter: u16,

    registers: CpuRegisters,
    stack_pointer: u16,

    interrupt_master_toggle: bool,
    interrupt_master_enabled: bool,
    halted: bool,
}

impl Cpu {
    pub fn new(memory: ConsoleMemory) -> Self {
        Self {
            memory: memory,
            program_counter: 0x0100,
            registers: CpuRegisters::new(),
            stack_pointer: 0,
            interrupt_master_toggle: false,
            interrupt_master_enabled: false,
            halted: false,
        }
    }

    pub fn tick(&mut self) {
        self.handle_interrupts();

        if self.interrupt_master_toggle {
            self.interrupt_master_enabled = true;
            self.interrupt_master_toggle = false;
        }

        if self.halted {
            self.memory.cycle_read(0x0000); // TODO proper dummy read???
            return;
        }

        let opcode = self.fetch_u8();
        OP_CODE_FUNCTION_TABLE[opcode as usize](self);
    }

    fn handle_interrupts(&mut self) {
        if !self.memory.interrupts.should_interrupt() {
            return;
        } else {
            self.halted = false;
        }

        if !self.interrupt_master_enabled {
            return;
        }

        if let Some(program_counter) = self.memory.interrupts.get_program_counter_address() {
            self.stack_push_u16(self.program_counter);
            self.interrupt_master_enabled = false;
            self.halted = false;
            self.program_counter = program_counter as u16;
        }
    }

    fn read(&mut self, address: u16) -> u8 {
        self.memory.cycle_read(address as usize)
    }

    fn write(&mut self, address: u16, value: u8) {
        self.memory.cycle_write(address as usize, value);
    }

    fn dummy_cycle(&mut self) {
        self.memory.tick();
    }

    fn read_hl(&mut self) -> u8 {
        self.read(self.registers.hl())
    }

    fn write_hl(&mut self, value: u8) {
        self.write(self.registers.hl(), value);
    }

    fn op_placeholder(&mut self) {
        panic!("Opcode not implemented!")
    }

    fn fetch_u8(&mut self) -> u8 {
        let value = self.read(self.program_counter);
        self.program_counter += 1;

        value
    }

    fn fetch_u16(&mut self) -> u16 {
        let lsb = self.fetch_u8();
        let msb = self.fetch_u8();

        (lsb as u16) | ((msb as u16) << 8)
    }

    fn stack_pop_u8(&mut self) -> u8 {
        let value = self.read(self.stack_pointer);
        self.stack_pointer = self.stack_pointer.wrapping_add(1);

        value
    }

    fn stack_push_u8(&mut self, value: u8) {
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
        self.write(self.stack_pointer, value);
    }

    fn stack_pop_u16(&mut self) -> u16 {
        // TODO check LSB/MSB order
        (self.stack_pop_u8() as u16) | ((self.stack_pop_u8() as u16) << 8)
    }

    fn stack_push_u16(&mut self, value: u16) {
        self.stack_push_u8((value >> 8) as u8);
        self.stack_push_u8((value & 0xFF) as u8);
    }

    // // TODO: as stack_pointer cannot be accessed, sync not needed
    // fn stack_push_u8(&mut self, msb: u8, lsb: u8) {
    //     self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    //     self.dummy_cycle();
    //     self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    //     self.write(self.stack_pointer.wrapping_add(1), msb);
    //     self.write(self.stack_pointer, lsb);
    // }

    // // TODO: as stack_pointer cannot be accessed, sync not needed
    // fn stack_pop_u8(&mut self) -> (u8, u8) {
    //     self.stack_pointer = self.stack_pointer.wrapping_add(1);
    //     let lsb = self.read(self.stack_pointer.wrapping_sub(1));
    //     self.stack_pointer = self.stack_pointer.wrapping_add(1);
    //     let msb = self.read(self.stack_pointer.wrapping_sub(1));

    //     return (msb, lsb);
    // }

    // fn stack_pop_u16(&mut self) -> u16 {
    //     let (msb, lsb) = self.stack_pop_u8();
    //     (lsb as u16) | ((msb as u16) << 8)
    // }

    // fn stack_push_u16(&mut self, value: u16) {
    //     self.stack_push_u8((value >> 8) as u8, (value & 0xFF) as u8);
    // }
}

impl Cpu {
    fn run_add_u8_and_update_flags(&mut self, operand: u8) {
        let result: u16 = (self.registers.a() as u16) + (operand as u16);
        self.registers.reset_status_flags();

        if (result & 0xFF) == 0 {
            self.registers.update_status_flags(status_flag::ZERO);
        }

        if ((self.registers.a() & 0xF) + (operand & 0xF)) > 0xF {
            self.registers.update_status_flags(status_flag::HALF_CARRY);
        }

        if result > 0xFF {
            self.registers.update_status_flags(status_flag::HALF_CARRY);
        }

        self.registers.set_a((result & 0xFF) as u8);
    }

    fn run_add_u16_and_update_flags(&mut self, operand: u16) {
        let hl = self.registers.hl();
        let result: u32 = (hl as u32) + (operand as u32);

        self.registers.remove_status_flags(
            status_flag::NEGATIVE | status_flag::CARRY | status_flag::HALF_CARRY,
        );

        if (((hl & 0xFFF) + (operand & 0xFFF)) & 0x1000) != 0 {
            self.registers.update_status_flags(status_flag::HALF_CARRY);
        }

        if (result & 0x10000) != 0 {
            self.registers.update_status_flags(status_flag::CARRY);
        }

        self.dummy_cycle();
        self.registers.set_hl((result & 0xFFFF) as u16);
    }

    fn run_adc_and_update_flags(&mut self, operand: u8) {
        let carry: u16 = if (self.registers.status_flags() & status_flag::CARRY) != 0 {
            1
        } else {
            0
        };

        // TODO wrapping add....
        let result: u16 = (self.registers.a() as u16) + (operand as u16) + carry;

        self.registers.reset_status_flags();

        if (result & 0xFF) == 0 {
            self.registers.update_status_flags(status_flag::ZERO);
        }

        if ((self.registers.a() & 0xF) + (operand & 0xF) + (carry as u8)) > 0xF {
            self.registers.update_status_flags(status_flag::HALF_CARRY);
        }

        if result > 0xFF {
            self.registers.update_status_flags(status_flag::CARRY);
        }

        self.registers.set_a((result & 0xFF) as u8);
    }

    fn run_sub_and_update_flags(&mut self, operand: u8) {
        self.registers.set_status_flags(status_flag::HALF_CARRY);

        if self.registers.a() == operand {
            self.registers.update_status_flags(status_flag::ZERO);
        }

        if (self.registers.a() & 0xF) < (operand & 0xF) {
            self.registers.update_status_flags(status_flag::HALF_CARRY);
        }

        if self.registers.a() < operand {
            self.registers.update_status_flags(status_flag::CARRY);
        }

        self.registers
            .set_a(self.registers.a().wrapping_sub(operand));
    }

    fn run_sbc_and_update_flags(&mut self, operand: u8) {
        let carry: u8 = if (self.registers.status_flags() & status_flag::CARRY) != 0 {
            1
        } else {
            0
        };

        let result = self.registers.a().wrapping_sub(operand).wrapping_sub(carry);

        self.registers.set_status_flags(status_flag::NEGATIVE);

        if result == 0 {
            self.registers.update_status_flags(status_flag::ZERO);
        }

        if (self.registers.a() & 0xF) < ((operand & 0xF) + carry) {
            self.registers.update_status_flags(status_flag::HALF_CARRY);
        }

        if (self.registers.a() as u16)
            .wrapping_sub(operand as u16)
            .wrapping_sub(carry as u16)
            > 0xFF
        {
            self.registers.update_status_flags(status_flag::CARRY);
        }

        self.registers.set_a(result);
    }

    fn run_and_and_update_flags(&mut self, operand: u8) {
        self.registers.set_a(self.registers.a() & operand);
        self.registers.set_status_flags(status_flag::HALF_CARRY);

        if self.registers.a() == 0 {
            self.registers.update_status_flags(status_flag::ZERO);
        }
    }

    fn run_xor_and_update_flags(&mut self, operand: u8) {
        self.registers.set_a(self.registers.a() ^ operand);
        self.registers.reset_status_flags();

        if self.registers.a() == 0 {
            self.registers.update_status_flags(status_flag::ZERO);
        }
    }

    fn run_or_and_update_flags(&mut self, operand: u8) {
        self.registers.set_a(self.registers.a() | operand);
        self.registers.reset_status_flags();

        if self.registers.a() == 0 {
            self.registers.update_status_flags(status_flag::ZERO);
        }
    }

    fn run_cp_and_update_flags(&mut self, operand: u8) {
        self.registers.set_status_flags(status_flag::NEGATIVE);

        if self.registers.a() == operand {
            self.registers.update_status_flags(status_flag::ZERO);
        }

        if (self.registers.a() & 0xF) < (operand & 0xF) {
            self.registers.update_status_flags(status_flag::HALF_CARRY);
        }

        if self.registers.a() < operand {
            self.registers.update_status_flags(status_flag::CARRY);
        }
    }

    fn run_inc_u8_and_update_flags(&mut self, operand: u8) -> u8 {
        let result = operand.wrapping_add(1);
        self.registers.remove_status_flags(
            status_flag::NEGATIVE | status_flag::ZERO | status_flag::HALF_CARRY,
        );

        if result == 0 {
            self.registers.update_status_flags(status_flag::ZERO);
        }

        if (result & 0xF) == 0 {
            self.registers.update_status_flags(status_flag::HALF_CARRY);
        }

        result
    }

    fn run_dec_u8_and_update_flags(&mut self, operand: u8) -> u8 {
        let result = operand.wrapping_sub(1);

        self.registers
            .remove_status_flags(status_flag::ZERO | status_flag::HALF_CARRY);
        self.registers.update_status_flags(status_flag::NEGATIVE);

        if result == 0 {
            self.registers.update_status_flags(status_flag::ZERO);
        }

        if (result & 0xF) == 0xF {
            self.registers.update_status_flags(status_flag::HALF_CARRY);
        }

        result
    }

    fn run_rlc_u8_and_update_flags(&mut self, operand: u8) -> u8 {
        let carry = (operand & 0x80) != 0;
        let result = operand.wrapping_shl(1) | carry as u8;

        self.registers.reset_status_flags();

        if carry {
            self.registers.update_status_flags(status_flag::CARRY);
        }

        if operand == 0 {
            self.registers.update_status_flags(status_flag::ZERO);
        }

        result
    }

    fn run_rrc_u8_and_update_flags(&mut self, operand: u8) -> u8 {
        let carry = (operand & 0x01) != 0;
        let result = operand.wrapping_shr(1) | ((carry as u8) << 7);

        self.registers.reset_status_flags();

        if carry {
            self.registers.update_status_flags(status_flag::CARRY);
        }

        if operand == 0 {
            self.registers.update_status_flags(status_flag::ZERO);
        }

        result
    }

    fn run_rl_u8_and_update_flags(&mut self, operand: u8) -> u8 {
        let carry = if (self.registers.status_flags() & status_flag::CARRY) != 0 {
            0x01
        } else {
            0x00
        };

        let mut result = operand.wrapping_shl(1) | carry;

        if (self.registers.status_flags() & status_flag::CARRY) != 0 {
            result |= 0x01;
        }

        self.registers.reset_status_flags();

        if (operand & 0x80) != 0 {
            self.registers.update_status_flags(status_flag::CARRY);
        }

        if result == 0 {
            self.registers.update_status_flags(status_flag::ZERO);
        }

        result
    }

    fn run_rr_u8_and_update_flags(&mut self, operand: u8) -> u8 {
        let carry = if (self.registers.status_flags() & status_flag::CARRY) != 0 {
            0x80
        } else {
            0x00
        };

        let result = operand.wrapping_shr(1) | carry;

        self.registers.reset_status_flags();

        if (operand & 0x01) != 0 {
            self.registers.update_status_flags(status_flag::CARRY);
        }

        if result == 0 {
            self.registers.update_status_flags(status_flag::ZERO);
        }

        result
    }

    fn run_sla_u8_and_update_flags(&mut self, operand: u8) -> u8 {
        let carry = (operand & 0x80) != 0;
        self.registers.reset_status_flags();

        if carry {
            self.registers.update_status_flags(status_flag::CARRY);
        }

        if (operand & 0x7F) == 0 {
            self.registers.update_status_flags(status_flag::ZERO);
        }

        operand.wrapping_shl(1)
    }

    fn run_sra_u8_and_update_flags(&mut self, operand: u8) -> u8 {
        let carry = (operand & 0x01) != 0;
        let result = operand.wrapping_shr(1) | (operand & 0x80);

        self.registers.reset_status_flags();

        if carry {
            self.registers.update_status_flags(status_flag::CARRY);
        }

        if result == 0 {
            self.registers.update_status_flags(status_flag::ZERO);
        }

        result
    }

    fn run_swap_u8_and_update_flags(&mut self, operand: u8) -> u8 {
        let result = (operand >> 4) | (operand << 4);
        self.registers.reset_status_flags();

        if result == 0 {
            self.registers.update_status_flags(status_flag::ZERO);
        }

        result
    }

    fn run_srl_u8_and_update_flags(&mut self, operand: u8) -> u8 {
        let result = operand >> 1;
        self.registers.reset_status_flags();

        if (operand & 0x01) != 0 {
            self.registers.update_status_flags(status_flag::CARRY);
        }

        if result == 0 {
            self.registers.update_status_flags(status_flag::ZERO);
        }

        result
    }

    fn run_bit_u8_and_update_flags(&mut self, operand: u8, bit: u8) {
        self.registers
            .set_status_flags(self.registers.status_flags() & status_flag::CARRY);
        self.registers.update_status_flags(status_flag::HALF_CARRY);

        if (operand & bit) == 0 {
            self.registers.update_status_flags(status_flag::ZERO);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::emulator::{
        memory::{ConsoleMemory, Memory},
        timer::ConsoleTimer,
    };

    use super::Cpu;

    fn run_test_rom(rom: &[u8]) {
        let mut logs = String::new();
        let mut cpu = Cpu::new(ConsoleMemory::new(rom, None));

        // while !logs.ends_with("Passed") && !logs.ends_with("Failed") {
        for _ in 0..1000000 {
            cpu.tick();

            if cpu.memory.last_address == 0xFF01 {
                logs.push(cpu.memory.silent_read(cpu.memory.last_address) as char);
            }
        }

        println!("logs={}", logs);

        if logs.contains("Failed") {
            panic!("Test failed\n{}", logs);
        }
    }

    #[test]
    fn test_rom_special() {
        let rom = include_bytes!("../../roms/gb-test-roms/cpu_instrs/individual/01-special.gb");
        run_test_rom(rom);
    }

    #[test]
    fn test_rom_interrupts() {
        let rom = include_bytes!("../../roms/gb-test-roms/cpu_instrs/individual/02-interrupts.gb");
        run_test_rom(rom);
    }

    #[test]
    fn test_rom_op_sp_hl() {
        let rom = include_bytes!("../../roms/gb-test-roms/cpu_instrs/individual/03-op sp,hl.gb");
        run_test_rom(rom);
    }

    #[test]
    fn test_rom_op_r_imm() {
        let rom = include_bytes!("../../roms/gb-test-roms/cpu_instrs/individual/04-op r,imm.gb");
        run_test_rom(rom);
    }

    #[test]
    fn test_rom_op_rp() {
        let rom = include_bytes!("../../roms/gb-test-roms/cpu_instrs/individual/05-op rp.gb");
        run_test_rom(rom);
    }

    #[test]
    fn test_rom_ld_r_r() {
        let rom = include_bytes!("../../roms/gb-test-roms/cpu_instrs/individual/06-ld r,r.gb");
        run_test_rom(rom);
    }

    #[test]
    fn test_rom_jr_jp_call_ret_rst() {
        let rom = include_bytes!(
            "../../roms/gb-test-roms/cpu_instrs/individual/07-jr,jp,call,ret,rst.gb"
        );
        run_test_rom(rom);
    }

    #[test]
    fn test_rom_misc() {
        let rom = include_bytes!("../../roms/gb-test-roms/cpu_instrs/individual/08-misc instrs.gb");
        run_test_rom(rom);
    }

    #[test]
    fn test_rom_op_r_r() {
        let rom = include_bytes!("../../roms/gb-test-roms/cpu_instrs/individual/09-op r,r.gb");
        run_test_rom(rom);
    }

    #[test]
    fn test_rom_bit() {
        let rom = include_bytes!("../../roms/gb-test-roms/cpu_instrs/individual/10-bit ops.gb");
        run_test_rom(rom);
    }

    #[test]
    fn test_rom_op_a_hl() {
        let rom = include_bytes!("../../roms/gb-test-roms/cpu_instrs/individual/11-op a,(hl).gb");
        run_test_rom(rom);
    }

    // #[test]
    // fn test_rom_interrupt_time() {
    //     let rom = include_bytes!("../../roms/gb-test-roms/interrupt_time/interrupt_time.gb");
    //     run_test_rom(rom);
    // }

    #[test]
    fn test_rom_interrupt_time() {
        let rom = include_bytes!("../../roms/gb-test-roms/instr_timing/instr_timing.gb");
        run_test_rom(rom);
    }
}
