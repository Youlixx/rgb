pub mod cpu;
pub mod memory;
pub mod timer;
pub mod interrupts;

pub trait Tickable<T> {
    fn tick(&mut self) -> T;
}