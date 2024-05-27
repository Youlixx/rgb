#[derive(Debug)]
pub struct Register {
    value: u16,
}

impl Default for Register {
    fn default() -> Self {
        Self { value: 0 }
    }
}

impl Register {
    pub fn set_msb(&mut self, msb: u8) {
        self.value &= 0x00FF;
        self.value |= (msb as u16) << 8;
    }

    pub fn set_lsb(&mut self, lsb: u8) {
        self.value &= 0xFF00;
        self.value |= lsb as u16;
    }

    pub fn msb(&self) -> u8 {
        (self.value >> 8) as u8
    }

    pub fn lsb(&self) -> u8 {
        (self.value & 0x00FF) as u8
    }
}

impl Into<u16> for Register {
    fn into(self) -> u16 {
        self.value
    }
}

impl From<u16> for Register {
    fn from(value: u16) -> Self {
        Self { value }
    }
}

#[derive(Debug)]
pub struct CpuRegisters {
    register_af: Register,
    register_bc: Register,
    register_de: Register,
    register_hl: Register,
}

impl CpuRegisters {
    pub fn new() -> Self {
        Self {
            register_af: Register { value: 0x0000 },
            register_bc: Register { value: 0x0000 },
            register_de: Register { value: 0x0000 },
            register_hl: Register { value: 0x0000 },
        }
    }

    pub fn a(&self) -> u8 {
        self.register_af.msb()
    }

    pub fn set_a(&mut self, value: u8) {
        self.register_af.set_msb(value);
    }

    pub fn b(&self) -> u8 {
        self.register_bc.msb()
    }

    pub fn set_b(&mut self, value: u8) {
        self.register_bc.set_msb(value);
    }

    pub fn c(&self) -> u8 {
        self.register_bc.lsb()
    }

    pub fn set_c(&mut self, value: u8) {
        self.register_bc.set_lsb(value);
    }

    pub fn d(&self) -> u8 {
        self.register_de.msb()
    }

    pub fn set_d(&mut self, value: u8) {
        self.register_de.set_msb(value);
    }

    pub fn e(&self) -> u8 {
        self.register_de.lsb()
    }

    pub fn set_e(&mut self, value: u8) {
        self.register_de.set_lsb(value);
    }

    pub fn h(&self) -> u8 {
        self.register_hl.msb()
    }

    pub fn set_h(&mut self, value: u8) {
        self.register_hl.set_msb(value);
    }

    pub fn l(&self) -> u8 {
        self.register_hl.lsb()
    }

    pub fn set_l(&mut self, value: u8) {
        self.register_hl.set_lsb(value);
    }

    pub fn af(&self) -> u16 {
        self.register_af.value
    }

    pub fn set_af(&mut self, value: u16) {
        self.register_af.value = value;
    }

    pub fn bc(&self) -> u16 {
        self.register_bc.value
    }

    pub fn set_bc(&mut self, value: u16) {
        self.register_bc.value = value;
    }

    pub fn de(&self) -> u16 {
        self.register_de.value
    }

    pub fn set_de(&mut self, value: u16) {
        self.register_de.value = value;
    }

    pub fn hl(&self) -> u16 {
        self.register_hl.value
    }

    pub fn set_hl(&mut self, value: u16) {
        self.register_hl.value = value;
    }

    pub fn status_flags(&self) -> u8 {
        self.register_af.lsb()
    }

    pub fn set_status_flags(&mut self, value: u8) {
        self.register_af.set_lsb(value);
    }

    pub fn reset_status_flags(&mut self) {
        self.set_status_flags(0);
    }

    pub fn update_status_flags(&mut self, flags: u8) {
        self.set_status_flags(self.status_flags() | flags);
    }

    pub fn remove_status_flags(&mut self, flags: u8) {
        self.set_status_flags(self.status_flags() & !flags);
    }
}

#[cfg(test)]
mod tests {
    use super::Register;

    #[test]
    fn test_register_set() {
        let mut register = Register { value: 0 };

        for value in 0..=0xFFFFu16 {
            let lsb = (value & 0xFF) as u8;
            let msb = (value >> 8) as u8;

            register.set_lsb(lsb);
            register.set_msb(msb);
            assert_eq!(register.value, value);
        }
    }

    #[test]
    fn test_register_get() {
        for value in 0..=0xFFFFu16 {
            let lsb = (value & 0xFF) as u8;
            let msb = (value >> 8) as u8;
            let register = Register { value };

            assert_eq!(register.lsb(), lsb);
            assert_eq!(register.msb(), msb);
        }
    }
}
