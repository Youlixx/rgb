// use super::{interrupts::InterruptSource, Tickable};

// #[derive(Debug)]
// struct TacRegister(u8);

// impl TacRegister {
//     fn is_enabled(&self) -> bool {
//         self.0 & 0x04 != 0
//     }

//     fn get_divider(&self) -> u8 {
//         match self.0 & 0x03 {
//             0x00 => 0x00,
//             0x01 => 0x04,
//             0x02 => 0x10,
//             0x03 => 0x40,
//             _ => unreachable!(),
//         }
//     }
// }

// #[derive(Debug)]
// struct SlowCounter {
//     counter: u8,
//     tick: u8,
//     reset: u8,
//     divider: u8,
// }

// impl SlowCounter {
//     fn new(divider: u8) -> Self {
//         Self {
//             counter: 0,
//             tick: 0,
//             reset: 0,
//             divider,
//         }
//     }
// }

// impl Tickable<bool> for SlowCounter {
//     fn tick(&mut self) -> bool {
//         self.tick = self.tick.wrapping_add(4);

//         if self.tick == self.divider {
//             self.tick = 0;

//             if self.counter == 0xFF {
//                 self.counter = self.reset;

//                 return true;
//             }

//             self.counter = self.counter.wrapping_add(1);
//         }

//         return false;
//     }
// }

// #[derive(Debug)]
// pub struct Timer {
//     divider: SlowCounter,
//     counter: SlowCounter,
//     control: TacRegister,
// }

// impl Timer {
//     pub fn new() -> Self {
//         Self {
//             divider: SlowCounter::new(0x40),
//             counter: SlowCounter::new(0x00),
//             control: TacRegister(0),
//         }
//     }

//     pub fn read_div(&self) -> u8 {
//         self.divider.counter
//     }

//     pub fn read_tima(&self) -> u8 {
//         self.counter.counter
//     }

//     pub fn read_tma(&self) -> u8 {
//         self.counter.reset
//     }

//     pub fn read_tac(&self) -> u8 {
//         self.control.0 & 0x07
//     }

//     pub fn reset_div(&mut self) {
//         self.divider.counter = 0;
//     }

//     pub fn write_tima(&mut self, value: u8) {
//         self.counter.counter = value;
//     }

//     pub fn write_tma(&mut self, reset_value: u8) {
//         self.counter.reset = reset_value;
//     }

//     pub fn write_tac(&mut self, control_flags: u8) {
//         self.control.0 = control_flags & 0x07;
//         self.counter.divider = self.control.get_divider();
//     }
// }

// impl Tickable<Option<InterruptSource>> for Timer {
//     #[must_use]
//     fn tick(&mut self) -> Option<InterruptSource> {
//         self.divider.tick();

//         if self.control.is_enabled() {
//             self.counter.tick().then_some(InterruptSource::Timer)
//         } else {
//             None
//         }
//     }
// }

// use super::{interrupts::InterruptSource};

// pub struct Timer {
//     divider: u16,
//     counter: u8,
//     modulo: u8,
//     control: u8,
//     tima_overflow: bool,
// }

// impl Timer {
//     pub fn new() -> Self {
//         Self {
//             divider: 0xABCC,
//             counter: 0x00,
//             modulo: 0x00,
//             control: 0x00,
//             tima_overflow: false,
//         }
//     }

//     pub fn read_div(&self) -> u8 {
//         (self.divider >> 8 & 0xFF) as u8
//     }

//     pub fn read_tima(&self) -> u8 {
//         self.counter
//     }

//     pub fn read_tma(&self) -> u8 {
//         self.modulo
//     }

//     pub fn read_tac(&self) -> u8 {
//         self.control & 0x07
//     }

//     pub fn reset_div(&mut self) {
//         self.divider = 0;
//     }

//     pub fn write_tima(&mut self, value: u8) {
//         self.counter = value;
//     }

//     pub fn write_tma(&mut self, reset_value: u8) {
//         self.modulo = reset_value;
//     }

//     pub fn write_tac(&mut self, control_flags: u8) {
//         self.control = control_flags & 0x07;
//     }
// }

// impl Tickable<Option<InterruptSource>> for Timer {
//     fn tick(&mut self) -> Option<InterruptSource> {
//         // u16 div = g_timer.div;
//         // u8 tac = read_timer(TIMER_TAC);

//         // // update DIV's 16bit value
//         // g_timer.div += ticks;

//         // // delayed IE
//         // if (g_cpu.ime_scheduled) {
//         //     interrupt_set_ime(true);
//         //     g_cpu.ime_scheduled = false;
//         // }

//         // // TIMA overflowed during the last cycle
//         // if (g_tima_overflow) {
//         //     g_tima_overflow = false;
//         //     interrupt_request(IV_TIMA);
//         //     g_timer.tima = g_timer.tma;
//         // }

//         // // We only update the timer's value at certain frequencies (freq_divider)
//         // // Here we compute the number of 'freq' between the old div and the new div
//         // // (in clocks, no cycles ! Hence we divide by 4)
//         // u16 freq = g_freq_divider[tac & 0x03] / 4;
//         // u8 increase_tima = ((div + ticks) / freq) - (div / freq);

//         // // If bit 2 of TAC is set to 0 then the timer is disabled
//         // if (increase_tima && tac & 0x4) {
//         //     u8 tima = read_timer(TIMER_TIMA);

//         //     if (tima == 0xFF) { // overflow
//         //         // Timer interrupt is delayed 1 cycle (4 clocks) from the TIMA
//         //         // overflow. The TMA reload to TIMA is also delayed. For one cycle,
//         //         // after overflowing TIMA, the value in TIMA is 00h, not TMA.
//         //         write_timer(TIMER_TIMA, 0x00);
//         //         g_tima_overflow = true;
//         //     } else {
//         //         write_timer(TIMER_TIMA, tima + increase_tima);
//         //     }
//         // }
//         let issue_interrupt = self.tima_overflow;

//         if issue_interrupt {
//             self.tima_overflow = false;
//             self.counter = self.modulo;
//         }

//         let frequency: u16 = match self.control & 0x3 {
//             0x00 => 0x100,
//             0x01 => 0x04,
//             0x02 => 0x10,
//             0x03 => 0x40,
//             _ => unreachable!(),
//         };

//         let increase_tima: u8 = (((self.divider.wrapping_add(1)) / frequency)
//             .wrapping_sub(self.divider / frequency)) as u8;

//         if (increase_tima != 0) && (self.control & 0x04) != 0 {
//             if self.counter == 0xFF {
//                 self.counter = 0;
//                 self.tima_overflow = true;
//             } else {
//                 self.counter = self.counter.wrapping_add(increase_tima);
//             }
//         }

//         self.divider = self.divider.wrapping_add(1);

//         issue_interrupt.then_some(InterruptSource::Timer)
//     }
// }

use super::{
    interrupts::{InterruptEmitter, InterruptSource},
    memory::Memory,
};

pub mod address {
    pub const DIV: usize = 0xFF04;
    pub const TIMA: usize = 0xFF05;
    pub const TMA: usize = 0xFF06;
    pub const TAC: usize = 0xFF07;
}

pub trait Timer: Memory + InterruptEmitter {}
impl<T: Memory + InterruptEmitter> Timer for T {}

pub struct ConsoleTimer {}

impl ConsoleTimer {
    pub fn new() -> Self {
        Self {}
    }
}

impl InterruptEmitter for ConsoleTimer {
    fn tick(&mut self) -> Option<InterruptSource> {
        None
    }
}

impl Memory for ConsoleTimer {
    fn silent_read(&self, address: usize) -> u8 {
        0
    }

    fn silent_write(&mut self, address: usize, value: u8) {}
}
