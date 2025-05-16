use super::memory::Memory;

/// Interrupt source enumeration.
#[repr(u8)]
#[derive(Debug)]
pub enum Interrupt {
    VBlank = 0x01,
    Lcd = 0x02,
    Timer = 0x04,
    Serial = 0x08,
    Joypad = 0x10,
}

/// Program counter address after the interrupt is acknowledged.
const INTERRUPT_ADDRESSES: [(Interrupt, u16); 5] = [
    (Interrupt::VBlank, 0x40),
    (Interrupt::Lcd, 0x48),
    (Interrupt::Timer, 0x50),
    (Interrupt::Serial, 0x58),
    (Interrupt::Joypad, 0x60),
];

mod address {
    pub const INTERRUPTS_FLAGS: u16 = 0xFF0F;
    pub const INTERRUPTS_ENABLE: u16 = 0xFFFF;
}

/// Interrupt registers, located at $FF0F and $FFFF.
pub struct InterruptRegisters {
    enable: u8,
    flags: u8,
}

impl InterruptRegisters {
    /// Initialize the interrupt registers in their power-up state.
    pub fn new() -> Self {
        Self {
            enable: 0xFF,
            flags: 0xFF,
        }
    }

    /// Set the interrupt bit in at $FFFF. If the given interrupt is enabled,
    /// it should be issued by calling `get_interrupt_address`, assuming it is
    /// the highest priority interrupt.
    pub fn update_flags(&mut self, flag: Interrupt) {
        self.flags |= flag as u8;
    }

    /// Check whether or not an enabled interrupt signal is pending.
    pub fn should_interrupt(&self) -> bool {
        // TODO not sure if it should be check against the 0x1F bitmask.
        (self.enable & self.flags) != 0
    }

    /// Get the interrupt jump address.
    pub fn get_interrupt_address(&mut self) -> Option<u16> {
        let interrupts = self.enable & self.flags;

        if interrupts & 0x1F == 0 {
            return None;
        }

        INTERRUPT_ADDRESSES
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

impl Memory for InterruptRegisters {
    fn read(&self, address: u16) -> u8 {
        match address {
            address::INTERRUPTS_ENABLE => self.enable,
            address::INTERRUPTS_FLAGS => self.flags,
            _ => unreachable!(),
        }
    }

    fn write(&mut self, address: u16, value: u8) {
        match address {
            address::INTERRUPTS_ENABLE => self.enable = value,
            address::INTERRUPTS_FLAGS => self.flags = value,
            _ => unreachable!(),
        }
    }
}
