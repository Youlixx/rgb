pub struct Console {
    memory: Vec<u8>,
}

impl Console {
    pub fn new() -> Self {
        Self {
            memory: vec![0; 0xFFFF],
        }
    }

    pub fn read(&self, address: u16) -> u8 {
        self.memory[address as usize]
    }

    pub fn write(&mut self, address: u16, value: u8) {
        self.memory[address as usize] = value
    }
}
