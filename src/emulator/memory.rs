use super::{
    interrupts::{self, Interrupts},
    timer::{self, ConsoleTimer, Timer},
};

pub trait Memory {
    fn silent_read(&self, address: usize) -> u8;
    fn silent_write(&mut self, address: usize, value: u8);
}

pub trait Tickable<T> {
    fn tick(&mut self) -> T;
}

struct DefaultSerialPort {}

impl Memory for DefaultSerialPort {
    fn silent_read(&self, address: usize) -> u8 {
        0
    }

    fn silent_write(&mut self, address: usize, value: u8) {}
}

pub struct ConsoleMemory {
    pub interrupts: Interrupts,
    pub last_address: usize, // TODO: temporary, for testing purposes, should be adapted using GB link dyn

    memory: Vec<u8>, // TODO: temporary, not everything needs to be mapped... + mirroring
    pub timer: Box<dyn Timer>,
    serial_port: Box<dyn Memory>,
}

impl ConsoleMemory {
    pub fn new(
        rom: &[u8],
        timer: Option<Box<dyn Timer>>,
        serial_port: Option<Box<dyn Memory>>,
    ) -> Self {
        let mut memory = vec![0; 0x10000];
        memory[..rom.len()].copy_from_slice(rom);

        Self {
            memory,
            last_address: 0x0000,
            interrupts: Interrupts::new(),
            timer: timer.unwrap_or(Box::new(ConsoleTimer::new())),
            serial_port: serial_port.unwrap_or(Box::new(DefaultSerialPort {})),
        }
    }

    pub fn tick(&mut self) {
        self.interrupts.update_flags(self.timer.tick());
    }

    pub fn cycle_read(&mut self, address: usize) -> u8 {
        self.last_address = address;
        self.tick();
        self.silent_read(address)
    }

    pub fn cycle_write(&mut self, address: usize, value: u8) {
        self.last_address = address;
        self.tick();
        self.silent_write(address, value);
    }
}

// TODO branching for IO registers, maybe separate struct for em
impl Memory for ConsoleMemory {
    fn silent_read(&self, address: usize) -> u8 {
        match address {
            0xFF01..=0xFF02 => self.serial_port.silent_read(address),
            timer::address::DIV..=timer::address::TAC => self.timer.silent_read(address),
            interrupts::address::INTERRUPTS_FLAGS => self.interrupts.read_flags(),
            interrupts::address::INTERRUPTS_ENABLE => self.interrupts.read_enable(),
            address => self.memory[address],
        }
    }

    fn silent_write(&mut self, address: usize, value: u8) {
        match address {
            0xFF01..=0xFF02 => self.serial_port.silent_write(address, value),
            timer::address::DIV..=timer::address::TAC => self.timer.silent_write(address, value),
            interrupts::address::INTERRUPTS_FLAGS => self.interrupts.write_flags(value),
            interrupts::address::INTERRUPTS_ENABLE => self.interrupts.write_enable(value),
            address => self.memory[address] = value,
        };
    }
}
