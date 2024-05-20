#[repr(u8)]
pub enum InterruptSource {
    VBlank = 0x01,
    Lcd = 0x02,
    Timer = 0x04,
    Serial = 0x08,
    Joypad = 0x10,
}

impl InterruptSource {
    const INTERRUPT_ADDRESSES: [(InterruptSource, usize); 5] = [
        (InterruptSource::VBlank, 0x40),
        (InterruptSource::Lcd, 0x48),
        (InterruptSource::Timer, 0x50),
        (InterruptSource::Serial, 0x58),
        (InterruptSource::Joypad, 0x60),
    ];
}

pub mod address {
    pub const INTERRUPTS_FLAGS: usize = 0xFF0F;
    pub const INTERRUPTS_ENABLE: usize = 0xFFFF;
}

#[derive(Debug)]
pub struct Interrupts {
    enable: u8,
    flags: u8,
}

impl Interrupts {
    pub fn new() -> Self {
        Self {
            enable: 0xFF,
            flags: 0xFF,
        }
    }

    pub fn update_flags(&mut self, flag: Option<InterruptSource>) {
        if let Some(flag) = flag {
            self.flags |= flag as u8;
        }
    }

    pub fn read_flags(&self) -> u8 {
        self.flags
    }

    pub fn read_enable(&self) -> u8 {
        self.enable
    }

    pub fn write_flags(&mut self, flags: u8) {
        self.flags = flags;
    }

    pub fn write_enable(&mut self, enable: u8) {
        self.enable = enable;
    }

    pub fn should_interrupt(&self) -> bool {
        (self.enable & self.flags) != 0
    }

    pub fn get_program_counter_address(&mut self) -> Option<usize> {
        let interrupts = self.enable & self.flags;

        if interrupts & 0x1F == 0 {
            return None;
        }

        InterruptSource::INTERRUPT_ADDRESSES
            .into_iter()
            .find_map(|(source, program_counter)| {
                let flag = source as u8;

                if self.flags & flag != 0 {
                    self.flags &= !flag;

                    Some(program_counter)
                } else {
                    None
                }
            })
    }
}

pub trait InterruptEmitter {
    fn tick(&mut self) -> Option<InterruptSource>;
}
