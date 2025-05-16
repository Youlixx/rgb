use super::interrupts::Interrupt;

/// Memory trait.
pub trait Memory {
    /// Read a value from the memory.
    ///
    /// The address is always given within the RELATIVE address space (starting at
    /// address 0x0), and the [`MemoryMap`] is responsible for ensuring the address
    /// validity, therefore it should never be out of bound.
    fn read(&self, address: u16) -> u8;

    /// Write a value to the memory.
    ///
    /// The address is always given within the RELATIVE address space (starting at
    /// address 0x0), and the [`MemoryMap`] is responsible for ensuring the address
    /// validity, therefore it should never be out of bound.
    fn write(&mut self, address: u16, value: u8);
}

/// Component trait.
pub trait Component: Memory {
    /// Tick the component.
    ///return
    /// Components may issue an interrupt to communicate to the CPU by returning the
    /// corresponding interrupt.
    fn tick(&mut self) -> Option<Interrupt> {
        None
    }
}

/// MemoryMap trait.
pub trait MemoryMap {
    /// Read a value from the memory.
    ///
    /// The address is always given within the console address space (16-bit address).
    /// This function should panic if the address is not mapped to any sub-memory.
    fn read(&self, address: u16) -> u8;

    /// Write a value to the memory.
    ///
    /// The address is always given within the console address space (16-bit address).
    /// This function should panic if the address is not mapped to any sub-memory.
    fn write(&mut self, address: u16, value: u8);

    /// Tick all the internal components.
    fn tick(&mut self);

    /// Check whether or not an enabled interrupt signal is pending.
    fn should_interrupt(&self) -> bool;

    /// Get the interrupt jump address.
    fn get_interrupt_address(&mut self) -> Option<u16>;

    /// Perform a read operation and tick the internal components.
    ///
    /// The components are always ticked first, the the read is performed. The address
    /// is always given within the console address space (16-bit address). This function
    /// should panic if the address is not mapped to any sub-memory.
    fn cycle_read(&mut self, address: u16) -> u8 {
        self.tick();
        self.read(address)
    }

    /// Perform a write operation and tick the internal components.
    ///
    /// The components are always ticked first, the the write is performed. The address
    /// is always given within the console address space (16-bit address). This function
    /// should panic if the address is not mapped to any sub-memory.
    fn cycle_write(&mut self, address: u16, value: u8) {
        self.tick();
        self.write(address, value);
    }
}

#[macro_export]
macro_rules! define_memory_map {
    (
        $MapName:ident,
        $($name:ident : $type:ty => $range:tt),* $(,)?
    ) => {
        pub struct $MapName {
            $($name: $type),*,
            interrupt_registers: InterruptRegisters,
        }

        impl $MapName {
            pub fn new($($name: $type),*) -> Self {
                Self {
                    $($name),*,
                    interrupt_registers: InterruptRegisters::new()
                }
            }
        }

        impl MemoryMap for $MapName {
            fn read(&self, address: u16) -> u8 {
                match address {
                    0xFF0F | 0xFFFF => self.interrupt_registers.read(address),
                    $(define_memory_map!(@make_pattern $range) => self.$name.read(address - define_memory_map!(@make_offset $range)),)*
                    #[allow(unreachable_patterns)]
                    _ => panic!("Tried to read from an unmapped address.")
                }
            }

            fn write(&mut self, address: u16, value: u8) {
                match address {
                    0xFF0F | 0xFFFF => self.interrupt_registers.write(address, value),
                    $(define_memory_map!(@make_pattern $range) => self.$name.write(address - define_memory_map!(@make_offset $range), value),)*
                    #[allow(unreachable_patterns)]
                    _ => panic!("Tried to write to an unmapped address.")
                }
            }

            fn tick(&mut self) {
                $(
                    if let Some(interrupt) = self.$name.tick() {
                        self.interrupt_registers.update_flags(interrupt);
                    }
                )*
            }

            fn should_interrupt(&self) -> bool {
                self.interrupt_registers.should_interrupt()
            }

            fn get_interrupt_address(&mut self) -> Option<u16> {
                self.interrupt_registers.get_interrupt_address()
            }
        }
    };

    (@make_pattern [ $start:expr ; $end:expr ]) => { $start..=$end };
    (@make_pattern $start:expr) => { $start };

    (@make_offset [ $start:expr ; $end:expr ]) => { $start };
    (@make_offset $start:expr) => { $start };
}

pub struct RawMemoryChunk<const SIZE: usize = 0x10000> {
    memory: [u8; SIZE],
}

impl<const SIZE: usize> RawMemoryChunk<SIZE> {
    pub fn new(rom: &[u8]) -> Self {
        let mut memory = [0u8; SIZE];
        memory[..rom.len()].copy_from_slice(rom);

        Self { memory }
    }
}

impl<const SIZE: usize> Memory for RawMemoryChunk<SIZE> {
    fn read(&self, address: u16) -> u8 {
        self.memory[address as usize]
    }

    fn write(&mut self, address: u16, value: u8) {
        self.memory[address as usize] = value;
    }
}

impl Component for RawMemoryChunk {}
