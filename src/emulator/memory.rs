use super::interrupts::Interrupt;

// TODO: rewrite docstrings...
/// Memory trait.
pub trait Memory {
    /// Read a value from the component memory. The address is always given in
    /// the absolute address space of the emulator. If the address is out of
    /// bound of the component memory, the function should return None.
    fn read(&self, address: usize) -> u8;

    /// Write a value to the component memory. The address is always given in
    /// the absolute address space of the emulator. If the address is out of
    /// bound of the component memory, the function should return None.
    fn write(&mut self, address: usize, value: u8);
}

/// Component trait.
pub trait Component: Memory {
    /// Tick the component. May return an interrupt to communicate to the CPU.
    fn tick(&mut self) -> Option<Interrupt> {
        None
    }
}

pub trait MemoryMap {
    fn read(&self, address: usize) -> u8;

    fn write(&mut self, address: usize, value: u8);

    fn tick(&mut self);

    /// Perform a read operation and tick the internals. If the address is not
    /// mapped by any component, this function will panic.
    fn cycle_read(&mut self, address: usize) -> u8 {
        self.tick();
        self.read(address)
    }

    /// Perform a write operation and tick the internals. If the address is not
    /// mapped by any component, this function will panic.
    fn cycle_write(&mut self, address: usize, value: u8) {
        self.tick();
        self.write(address, value);
    }

    /// Check whether or not an enabled interrupt signal is pending.
    fn should_interrupt(&self) -> bool;

    /// Get the interrupt jump address.
    fn get_interrupt_address(&mut self) -> Option<usize>;
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
            fn read(&self, address: usize) -> u8 {
                match address {
                    0xFF0F | 0xFFFF => self.interrupt_registers.read(address),
                    $(define_memory_map!(@make_pattern $range) => self.$name.read(address - define_memory_map!(@make_offset $range)),)*
                    _ => panic!("Tried to read from an unmapped address.")
                }
            }

            fn write(&mut self, address: usize, value: u8) {
                match address {
                    0xFF0F | 0xFFFF => self.interrupt_registers.write(address, value),
                    $(define_memory_map!(@make_pattern $range) => self.$name.write(address - define_memory_map!(@make_offset $range), value),)*
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

            fn get_interrupt_address(&mut self) -> Option<usize> {
                self.interrupt_registers.get_interrupt_address()
            }
        }
    };

    (@make_pattern [ $start:expr ; $end:expr ]) => { $start..$end };
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
    fn read(&self, address: usize) -> u8 {
        self.memory[address]
    }

    fn write(&mut self, address: usize, value: u8) {
        self.memory[address] = value;
    }
}

impl Component for RawMemoryChunk {}
