use bitmask_enum::bitmask;

#[bitmask(u8)]
pub enum InterruptFlags {
    VBlank = 0x01,
    Lcd = 0x02,
    Timer = 0x04,
    Serial = 0x08,
    Joypad = 0x10,
}

pub struct Interrupts {
    enable: InterruptFlags,
    flags: InterruptFlags,
}

impl Interrupts {
    const PROGRAM_COUNTER_VBLANK: usize = 0x40;
    const PROGRAM_COUNTER_LCD: usize = 0x40;
    const PROGRAM_COUNTER_TIMER: usize = 0x40;
    const PROGRAM_COUNTER_SERIAL: usize = 0x40;
    const PROGRAM_COUNTER_JOYPAD: usize = 0x40;

    pub fn new() -> Self {
        Self {
            enable: InterruptFlags::none(),
            flags: InterruptFlags::none(),
        }
    }

    pub fn update_flags(&mut self, flag: Option<InterruptFlags>) {
        if let Some(flag) = flag {
            self.flags |= flag;
        }
    }

    pub fn read_flags(&self) -> u8 {
        self.flags.bits
    }

    pub fn read_enable(&self) -> u8 {
        self.enable.bits
    }

    pub fn write_flags(&mut self, flags: u8) {
        self.flags.bits = flags;
    }

    pub fn write_enable(&mut self, enable: u8) {
        self.enable.bits = enable;
    }

    pub fn get_program_counter_address(&mut self) -> Option<usize> {
        let interrupts = self.enable & self.flags;

        if interrupts.is_none() {
            return None;
        }

        if interrupts.contains(InterruptFlags::VBlank) {
            self.flags &= !InterruptFlags::VBlank;
            return Some(Interrupts::PROGRAM_COUNTER_VBLANK);
        } else if interrupts.contains(InterruptFlags::Lcd) {
            self.flags &= !InterruptFlags::Lcd;
            return Some(Interrupts::PROGRAM_COUNTER_LCD);
        } else if interrupts.contains(InterruptFlags::Timer) {
            self.flags &= !InterruptFlags::Timer;
            return Some(Interrupts::PROGRAM_COUNTER_TIMER);
        } else if interrupts.contains(InterruptFlags::Serial) {
            self.flags &= !InterruptFlags::Serial;
            return Some(Interrupts::PROGRAM_COUNTER_SERIAL);
        } else if interrupts.contains(InterruptFlags::Joypad) {
            self.flags &= !InterruptFlags::Joypad;
            return Some(Interrupts::PROGRAM_COUNTER_JOYPAD);
        }

        unreachable!()
    }
}
