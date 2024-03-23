pub trait Memory {
    fn read(&self, address: u16) -> u8;
    fn write(&mut self, address: u16, value: u8);
}

pub struct MemoryController {
    memory: Vec<u8>,
}

impl MemoryController {
    pub fn new() -> Self {
        Self {
            memory: vec![0; 0xFFFF],
        }
    }
}

impl Memory for MemoryController {
    fn read(&self, address: u16) -> u8 {
        self.memory[address as usize]
    }

    fn write(&mut self, address: u16, value: u8) {
        self.memory[address as usize] = value;
    }
}
