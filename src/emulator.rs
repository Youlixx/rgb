pub mod cpu;
pub mod interrupts;
pub mod memory;
pub mod timer;

pub trait Tickable<T> {
    fn tick(&mut self) -> T;
}
