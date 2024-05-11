pub trait MemoryController {
    fn read(&self, address: u16) -> u8;
    fn write(&self, address: u16, value: u8);
}
