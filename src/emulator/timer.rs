use super::{
    interrupts::Interrupt,
    memory::{Component, Memory},
};

pub mod address {
    pub const DIV: u16 = 0xFF04;
    pub const TIMA: u16 = 0xFF05;
    pub const TMA: u16 = 0xFF06;
    pub const TAC: u16 = 0xFF07;
}

pub struct ConsoleTimer {
    divider: u16,
    counter: u8,
    modulo: u8,
    control: u8,
    tima_overflow: bool,
}

impl ConsoleTimer {
    pub fn new() -> Self {
        Self {
            divider: 0xABCC,
            counter: 0x00,
            modulo: 0x00,
            control: 0x00,
            tima_overflow: false,
        }
    }
}

impl Memory for ConsoleTimer {
    fn read(&self, address: u16) -> u8 {
        match address {
            address::DIV => (self.divider >> 8) as u8,
            address::TIMA => self.counter,
            address::TMA => self.modulo,
            address::TAC => self.control & 0x07,
            _ => unreachable!(),
        }
    }

    fn write(&mut self, address: u16, value: u8) {
        match address {
            address::DIV => self.divider = 0,
            address::TIMA => self.counter = value,
            address::TMA => self.modulo = value,
            address::TAC => self.control = value & 0x07,
            _ => unreachable!(),
        };
    }
}

impl Component for ConsoleTimer {
    fn tick(&mut self) -> Option<Interrupt> {
        let issue_interrupt = self.tima_overflow;

        if issue_interrupt {
            self.tima_overflow = false;
            self.counter = self.modulo;
        }

        let frequency: u16 = match self.control & 0x3 {
            0x00 => 0x100,
            0x01 => 0x04,
            0x02 => 0x10,
            0x03 => 0x40,
            _ => unreachable!(),
        };

        let increase_tima: u8 = (((self.divider.wrapping_add(1)) / frequency)
            .wrapping_sub(self.divider / frequency)) as u8;

        if (increase_tima != 0) && (self.control & 0x04) != 0 {
            if self.counter == 0xFF {
                self.counter = 0;
                self.tima_overflow = true;
            } else {
                self.counter = self.counter.wrapping_add(increase_tima);
            }
        }

        self.divider = self.divider.wrapping_add(1);

        issue_interrupt.then_some(Interrupt::Timer)
    }
}
