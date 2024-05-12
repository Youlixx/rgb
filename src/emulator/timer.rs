use super::{interrupts::InterruptFlags, Tickable};

struct TacRegister(u8);

impl TacRegister {
    fn is_enabled(&self) -> bool {
        self.0 & 0x04 != 0
    }

    fn get_divider(&self) -> u8 {
        match self.0 & 0x03 {
            0x00 => 0x00,
            0x01 => 0x04,
            0x02 => 0x10,
            0x03 => 0x40,
            _ => unreachable!(),
        }
    }
}

struct SlowCounter {
    counter: u8,
    tick: u8,
    reset: u8,
    divider: u8,
}

impl SlowCounter {
    fn new(divider: u8) -> Self {
        Self {
            counter: 0,
            tick: 0,
            reset: 0,
            divider,
        }
    }
}

impl Tickable<bool> for SlowCounter {
    fn tick(&mut self) -> bool {
        self.tick = self.tick.wrapping_add(1);

        if self.tick == self.divider {
            self.tick = 0;

            if self.counter == 0xFF {
                self.counter = self.reset;

                return true;
            }

            self.counter = self.counter.wrapping_add(1);
        }

        return false;
    }
}

pub struct Timer {
    divider: SlowCounter,
    counter: SlowCounter,
    control: TacRegister,
}

impl Timer {
    pub fn new() -> Self {
        Self {
            divider: SlowCounter::new(0x40),
            counter: SlowCounter::new(0x00),
            control: TacRegister(0),
        }
    }

    pub fn read_div(&self) -> u8 {
        self.divider.counter
    }

    pub fn read_tima(&self) -> u8 {
        self.counter.counter
    }

    pub fn read_tma(&self) -> u8 {
        self.counter.reset
    }

    pub fn read_tac(&self) -> u8 {
        self.control.0
    }

    pub fn reset_div(&mut self) {
        self.divider.counter = 0;
    }

    pub fn write_tma(&mut self, reset_value: u8) {
        self.counter.reset = reset_value;
    }

    pub fn write_tac(&mut self, control_flags: u8) {
        self.control.0 = control_flags;
        self.counter.divider = self.control.get_divider();
    }
}

impl Tickable<Option<InterruptFlags>> for Timer {
    #[must_use]
    fn tick(&mut self) -> Option<InterruptFlags> {
        self.divider.tick();

        if self.control.is_enabled() {
            self.counter.tick().then_some(InterruptFlags::Timer)
        } else {
            None
        }
    }
}
