use super::{interrupts::Interrupts, timer::Timer, Tickable as _};

pub trait Memory {
    fn silent_read(&self, address: usize) -> u8;
    fn silent_write(&mut self, address: usize, value: u8);
}

pub trait TickMemory: Memory {
    fn cycle_read(&mut self, address: usize) -> u8 {
        self.silent_read(address)
    }

    fn cycle_write(&mut self, address: usize, value: u8) {
        self.silent_write(address, value)
    }
}

pub struct ConsoleMemory {
    pub interrupts: Interrupts,
    pub last_address: usize, // TODO: temporary, for testing purposes.

    memory: Vec<u8>, // TODO: temporary, not everything needs to be mapped... + mirroring
    pub timer: Timer,
}

impl ConsoleMemory {
    const ADDRESS_TIMER_DIV: usize = 0xFF04;
    const ADDRESS_TIMER_TIMA: usize = 0xFF05;
    const ADDRESS_TIMER_TMA: usize = 0xFF06;
    const ADDRESS_TIMER_TAC: usize = 0xFF07;

    const ADDRESS_INTERRUPTS_FLAGS: usize = 0xFF0F;
    const ADDRESS_INTERRUPTS_ENABLE: usize = 0xFFFF;

    pub fn new(rom: &[u8]) -> Self {
        let mut memory = vec![0; 0x10000];
        memory[..rom.len()].copy_from_slice(rom);

        Self {
            memory,
            last_address: 0x0000,
            interrupts: Interrupts::new(),
            timer: Timer::new(),
        }
    }
}

impl Memory for ConsoleMemory {
    fn silent_read(&self, address: usize) -> u8 {
        match address {
            ConsoleMemory::ADDRESS_TIMER_DIV => self.timer.read_div(),
            ConsoleMemory::ADDRESS_TIMER_TIMA => self.timer.read_tima(),
            ConsoleMemory::ADDRESS_TIMER_TMA => self.timer.read_tma(),
            ConsoleMemory::ADDRESS_TIMER_TAC => self.timer.read_tac(),
            ConsoleMemory::ADDRESS_INTERRUPTS_FLAGS => self.interrupts.read_flags(),
            ConsoleMemory::ADDRESS_INTERRUPTS_ENABLE => self.interrupts.read_enable(),
            address => self.memory[address],
        }
    }

    fn silent_write(&mut self, address: usize, value: u8) {
        match address {
            ConsoleMemory::ADDRESS_TIMER_DIV => self.timer.reset_div(),
            ConsoleMemory::ADDRESS_TIMER_TIMA => (),
            ConsoleMemory::ADDRESS_TIMER_TMA => self.timer.write_tma(value),
            ConsoleMemory::ADDRESS_TIMER_TAC => self.timer.write_tac(value),
            ConsoleMemory::ADDRESS_INTERRUPTS_FLAGS => self.interrupts.write_flags(value),
            ConsoleMemory::ADDRESS_INTERRUPTS_ENABLE => self.interrupts.write_enable(value),
            address => self.memory[address] = value,
        }
    }
}

impl TickMemory for ConsoleMemory {
    fn cycle_read(&mut self, address: usize) -> u8 {
        self.last_address = address;
        self.interrupts.update_flags(self.timer.tick());
        self.silent_read(address)
    }

    fn cycle_write(&mut self, address: usize, value: u8) {
        self.last_address = address;
        self.interrupts.update_flags(self.timer.tick());
        self.silent_write(address, value);
    }
}
