use super::interrupts::{Interrupt, InterruptRegisters};

/// Memory trait.
pub trait Memory {
    /// Read a value from the component memory. The address is always given in
    /// the absolute address space of the emulator. If the address is out of
    /// bound of the component memory, the function should return None.
    fn read(&self, address: usize) -> Option<u8>;

    /// Write a value to the component memory. The address is always given in
    /// the absolute address space of the emulator. If the address is out of
    /// bound of the component memory, the function should return None.
    fn write(&mut self, address: usize, value: u8) -> bool;
}

/// Component trait.
pub trait Component: Memory {
    /// Tick the component. May return an interrupt to communicate to the CPU.
    fn tick(&mut self) -> Option<Interrupt>;
}

/// Inhert memories can be tickable, but they never issue any interrupt.
impl<T> Component for T
where
    T: Memory,
{
    /// Since the component is a simple memory, it never issue any interrupt.
    fn tick(&mut self) -> Option<Interrupt> {
        None
    }
}

/// Console memory map.
pub struct MemoryMap {
    /// Console components (e.g. memory, PPU, APU, ...)
    components: Vec<Box<dyn Component>>,

    /// Console interrupt registers, always present.
    /// The components should never overlap with $0xFF0F and $0xFFFF or the
    /// interrupt will not be triggered.
    pub interrupt_registers: InterruptRegisters,
}

impl MemoryMap {
    pub fn new(components: Vec<Box<dyn Component>>) -> Self {
        Self {
            components,
            interrupt_registers: InterruptRegisters::new(),
        }
    }

    /// Tick the component and issue interrupts.
    pub fn tick(&mut self) {
        self.components
            .iter_mut()
            .filter_map(|component| component.tick())
            .for_each(|interrupt| self.interrupt_registers.update_flags(interrupt));
    }

    /// Perform a 'silent' read. Directly read to the memory without ticking
    /// the internals, this function is exposed for debug purposes, but may
    /// corrupt the emulation state. If the address is not mapped by any
    /// component, this function will panic.
    pub fn read(&self, address: usize) -> u8 {
        self.components
            .iter()
            .find_map(|memory| memory.read(address))
            .or_else(|| self.interrupt_registers.read(address))
            .expect("Tried to read from an unmapped area of the memory.")
    }

    /// Perform a 'silent' write. Directly write to the memory without ticking
    /// the internals, this function is exposed for debug purposes, but may
    /// corrupt the emulation state. If the address is not mapped by any
    /// component, this function will panic.
    pub fn write(&mut self, address: usize, value: u8) {
        if !(self
            .components
            .iter_mut()
            .any(|memory| memory.write(address, value))
            || self.interrupt_registers.write(address, value))
        {
            panic!("Tried to write to an unmapped area of the memory.")
        }
    }

    /// Perform a read operation and tick the internals. If the address is not
    /// mapped by any component, this function will panic.
    pub fn cycle_read(&mut self, address: usize) -> u8 {
        self.tick();
        self.read(address)
    }

    /// Perform a write operation and tick the internals. If the address is not
    /// mapped by any component, this function will panic.
    pub fn cycle_write(&mut self, address: usize, value: u8) {
        self.tick();
        self.write(address, value);
    }
}

impl Default for MemoryMap {
    /// Returns the "default value" for a type.
    fn default() -> Self {
        Self::new(vec![])
    }
}
