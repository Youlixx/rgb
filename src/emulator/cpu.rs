mod cb_codes;
mod op_codes;
mod register;

use self::register::CpuRegisters;

use super::memory::MemoryMap;

mod status_flag {
    pub const ZERO: u8 = 0x80;
    pub const NEGATIVE: u8 = 0x40;
    pub const HALF_CARRY: u8 = 0x20;
    pub const CARRY: u8 = 0x10;
}

pub struct Cpu<M: MemoryMap> {
    memory: M,
    program_counter: u16,

    registers: CpuRegisters,
    stack_pointer: u16,

    interrupt_master_toggle: bool,
    interrupt_master_enabled: bool,
    halted: bool,
}

impl<M: MemoryMap> Cpu<M> {
    pub fn new(memory: M) -> Self {
        Self {
            memory,
            program_counter: 0x0100,
            registers: CpuRegisters::new(),
            stack_pointer: 0xFFF4,
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
            self.dummy_cycle();
            return;
        }

        let opcode = self.fetch_u8();
        Self::OP_CODE_FUNCTION_TABLE[opcode as usize](self);
    }

    fn handle_interrupts(&mut self) {
        if !self.memory.should_interrupt() {
            return;
        } else {
            self.halted = false;
        }

        if !self.interrupt_master_enabled {
            return;
        }

        if let Some(program_counter) = self.memory.get_interrupt_address() {
            self.stack_push(self.program_counter);
            self.interrupt_master_enabled = false;
            self.halted = false;
            self.program_counter = program_counter;
        }
    }

    fn read(&mut self, address: u16) -> u8 {
        self.memory.cycle_read(address)
    }

    fn write(&mut self, address: u16, value: u8) {
        self.memory.cycle_write(address, value);
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

    // TODO move elsewhere
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

    fn stack_push(&mut self, value: u16) {
        self.dummy_cycle();
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
        self.write(self.stack_pointer, (value >> 8) as u8);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
        self.write(self.stack_pointer, (value & 0xFF) as u8);
    }

    fn stack_pop(&mut self) -> u16 {
        let lsb = self.read(self.stack_pointer);
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        let msb = self.read(self.stack_pointer);
        self.stack_pointer = self.stack_pointer.wrapping_add(1);

        (lsb as u16) | ((msb as u16) << 8)
    }
}

// TODO move elsewhere
impl<M: MemoryMap> Cpu<M> {
    fn run_add_u8_and_update_flags(&mut self, operand: u8) {
        let result: u16 = (self.registers.a() as u16).wrapping_add(operand as u16);
        self.registers.reset_status_flags();

        if (result & 0xFF) == 0 {
            self.registers.update_status_flags(status_flag::ZERO);
        }

        if ((self.registers.a() & 0xF) + (operand & 0xF)) > 0xF {
            self.registers.update_status_flags(status_flag::HALF_CARRY);
        }

        if result > 0xFF {
            self.registers.update_status_flags(status_flag::CARRY);
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

        let result: u16 = (self.registers.a() as u16)
            .wrapping_add(operand as u16)
            .wrapping_add(carry);

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
    use std::{cell::RefCell, rc::Rc};

    use crate::{
        define_memory_map,
        emulator::{
            interrupts::{Interrupt, InterruptRegisters},
            memory::{Component, Memory, MemoryMap, RawMemoryChunk},
            timer::ConsoleTimer,
        },
    };

    use super::Cpu;

    pub struct CycleCounter(u8);

    impl Memory for CycleCounter {
        fn read(&self, _: u16) -> u8 {
            self.0
        }

        fn write(&mut self, _: u16, _: u8) {}
    }

    impl Component for CycleCounter {
        fn tick(&mut self) -> Option<Interrupt> {
            self.0 = self.0.wrapping_add(1);
            None
        }
    }

    define_memory_map!(
        CycleCountedMemoryMap,
        counter: CycleCounter => 0x00FF,
        memory: RawMemoryChunk => [0x0000; 0xFFFF]
    );

    pub fn new_cycle_counted_cpu(rom: &[u8], offset: u8) -> Cpu<CycleCountedMemoryMap> {
        Cpu::new(CycleCountedMemoryMap::new(
            CycleCounter(0u8.wrapping_sub(offset)),
            RawMemoryChunk::new(rom),
        ))
    }

    pub struct TestSerialPort {
        logs: String,
        completed: bool,
    }

    impl Memory for Rc<RefCell<TestSerialPort>> {
        fn read(&self, _: u16) -> u8 {
            0
        }

        fn write(&mut self, _: u16, value: u8) {
            let mut logger = self.borrow_mut();
            logger.logs.push(value as char);

            if logger.logs.ends_with("Passed") || logger.logs.ends_with("Failed") {
                logger.completed = true;
            }
        }
    }

    impl Component for Rc<RefCell<TestSerialPort>> {}

    impl TestSerialPort {
        fn new() -> Rc<RefCell<Self>> {
            Rc::new(RefCell::new(Self {
                logs: String::new(),
                completed: false,
            }))
        }
    }

    define_memory_map!(
        TestMemoryMap,
        logger: Rc<RefCell<TestSerialPort>> => 0xFF01,
        timer: ConsoleTimer => [0xFF04; 0xFF08],
        memory: RawMemoryChunk => [0x0000; 0xFFFF]
    );

    fn run_test_rom(rom: &[u8]) {
        let logger = TestSerialPort::new();
        let mut cpu = Cpu::new(TestMemoryMap::new(
            logger.clone(),
            ConsoleTimer::new(),
            RawMemoryChunk::new(rom),
        ));

        while !logger.borrow().completed {
            cpu.tick();
        }

        let logs = logger.borrow().logs.clone();
        assert!(logs.contains("Passed"), "Test rom failed\n{}", logs);
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

    #[test]
    fn test_rom_instruction_timing() {
        let rom = include_bytes!("../../roms/gb-test-roms/instr_timing/instr_timing.gb");
        run_test_rom(rom);
    }

    #[test]
    fn test_rom_mem_reads() {
        let rom = include_bytes!("../../roms/gb-test-roms/mem_timing/individual/01-read_timing.gb");
        run_test_rom(rom);
    }

    #[test]
    fn test_rom_mem_writes() {
        let rom =
            include_bytes!("../../roms/gb-test-roms/mem_timing/individual/02-write_timing.gb");
        run_test_rom(rom);
    }

    #[test]
    fn test_rom_mem_modify() {
        let rom =
            include_bytes!("../../roms/gb-test-roms/mem_timing/individual/03-modify_timing.gb");
        run_test_rom(rom);
    }
}
