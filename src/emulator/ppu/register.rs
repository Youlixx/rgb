use std::ops::Index;

use crate::emulator::memory::Memory;

use super::{Color, Mode};

pub mod address {
    pub const LCDC: u16 = 0xFF40;
    pub const STAT: u16 = 0xFF41;
    pub const SCY: u16 = 0xFF42;
    pub const SCX: u16 = 0xFF43;
    pub const LY: u16 = 0xFF44;
    pub const LYC: u16 = 0xFF45;
    pub const BGP: u16 = 0xFF47;
    pub const OBP0: u16 = 0xFF48;
    pub const OBP1: u16 = 0xFF49;
    pub const WY: u16 = 0xFF4A;
    pub const WX: u16 = 0xFF4B;
}

mod control_flags {
    pub const LCD_AND_PPU_ENABLED: u8 = 0x80;
    pub const WINDOW_TILE_MAP: u8 = 0x40;
    pub const WINDOW_ENABLED: u8 = 0x20;
    pub const TILE_ADDRESSING_MODE: u8 = 0x10;
    pub const BACKGROUND_TILE_MAP: u8 = 0x08;
    pub const SPRITE_SIZE: u8 = 0x04;
    pub const SPRITE_ENABLED: u8 = 0x02;
    pub const BACKGROUND_ENABLED: u8 = 0x01;
}

mod stat_flags {
    pub const LYC_INTERRUPT_SELECT: u8 = 0x40;
    pub const MODE_2_INTERRUPT_SELECT: u8 = 0x20;
    pub const MODE_1_INTERRUPT_SELECT: u8 = 0x10;
    pub const MODE_0_INTERRUPT_SELECT: u8 = 0x08;
    pub const LYC_COMPARE: u8 = 0x04;
    pub const PPU_MODE: u8 = 0x03;
}

pub enum TileMapMode {
    Tiles0x9800,
    Tiles0x9C00,
}

pub enum TileAddressingMode {
    Method0x8000,
    Method0x8800,
}

#[derive(Debug, Default, Clone, Eq, PartialEq)]
pub struct ControlRegister {
    flags: u8,
}

impl ControlRegister {
    pub fn is_lcd_and_ppu_enabled(&self) -> bool {
        self.flags & control_flags::LCD_AND_PPU_ENABLED != 0
    }

    pub fn window_tile_map(&self) -> TileMapMode {
        if self.flags & control_flags::WINDOW_TILE_MAP != 0 {
            TileMapMode::Tiles0x9C00
        } else {
            TileMapMode::Tiles0x9800
        }
    }

    pub fn is_window_enabled(&self) -> bool {
        self.flags & control_flags::WINDOW_ENABLED != 0
    }

    pub fn tile_addressing_mode(&self) -> TileAddressingMode {
        if self.flags & control_flags::TILE_ADDRESSING_MODE != 0 {
            TileAddressingMode::Method0x8000
        } else {
            TileAddressingMode::Method0x8800
        }
    }

    pub fn background_tile_map(&self) -> TileMapMode {
        if self.flags & control_flags::BACKGROUND_TILE_MAP != 0 {
            TileMapMode::Tiles0x9C00
        } else {
            TileMapMode::Tiles0x9800
        }
    }

    pub fn is_using_large_sprites(&self) -> bool {
        self.flags & control_flags::SPRITE_SIZE != 0
    }

    pub fn is_foreground_enabled(&self) -> bool {
        self.flags & control_flags::SPRITE_ENABLED != 0
    }

    pub fn is_background_enabled(&self) -> bool {
        self.flags & control_flags::BACKGROUND_ENABLED != 0
    }
}

#[derive(Debug, Default, Clone, Eq, PartialEq)]
pub struct InterruptSources {
    scanline_interrupt: bool,
    mode_0_interrupt: bool,
    mode_1_interrupt: bool,
    mode_2_interrupt: bool,
}

impl InterruptSources {
    fn get_register_value(&self) -> u8 {
        let mut value = 0;

        if self.scanline_interrupt {
            value |= stat_flags::LYC_INTERRUPT_SELECT;
        }

        if self.mode_0_interrupt {
            value |= stat_flags::MODE_0_INTERRUPT_SELECT;
        }

        if self.mode_1_interrupt {
            value |= stat_flags::MODE_1_INTERRUPT_SELECT;
        }

        if self.mode_2_interrupt {
            value |= stat_flags::MODE_2_INTERRUPT_SELECT;
        }

        value
    }

    fn from_register_value(&mut self, value: u8) {
        self.scanline_interrupt = value & stat_flags::LYC_INTERRUPT_SELECT != 0;
        self.mode_0_interrupt = value & stat_flags::MODE_0_INTERRUPT_SELECT != 0;
        self.mode_1_interrupt = value & stat_flags::MODE_1_INTERRUPT_SELECT != 0;
        self.mode_2_interrupt = value & stat_flags::MODE_2_INTERRUPT_SELECT != 0;
    }

    pub fn scanline_interrupt(&self) -> bool {
        self.scanline_interrupt
    }

    pub fn mode_0_interrupt(&self) -> bool {
        self.mode_0_interrupt
    }

    pub fn mode_1_interrupt(&self) -> bool {
        self.mode_1_interrupt
    }

    pub fn mode_2_interrupt(&self) -> bool {
        self.mode_2_interrupt
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PaletteRegister {
    colors: [Color; 4],
}

impl PaletteRegister {
    fn get_register_value(&self) -> u8 {
        let mut value = 0;

        for (index, color) in self.colors.iter().enumerate() {
            value |= match color {
                Color::White => 0u8,
                Color::LightGray => 1u8,
                Color::DarkGray => 2u8,
                Color::Black => 3u8,
            } << (index * 2);
        }

        value
    }

    fn from_register_value(&mut self, value: u8) {
        for (index, color) in self.colors.iter_mut().enumerate() {
            *color = match (value >> (index * 2)) & 0x03 {
                0 => Color::White,
                1 => Color::LightGray,
                2 => Color::DarkGray,
                3 => Color::Black,
                _ => unreachable!(),
            };
        }
    }
}

impl Default for PaletteRegister {
    fn default() -> Self {
        Self {
            colors: [
                Color::White,
                Color::LightGray,
                Color::DarkGray,
                Color::Black,
            ],
        }
    }
}

impl Index<usize> for PaletteRegister {
    type Output = Color;

    fn index(&self, index: usize) -> &Self::Output {
        &self.colors[index]
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Registers {
    pub mode: Mode,

    control: ControlRegister,
    interrupts: InterruptSources,
    background_palette: PaletteRegister,
    sprite_palettes: (PaletteRegister, PaletteRegister),
    scanline: u8,
    scroll_x: u8,
    scroll_y: u8,
    window_x: u8,
    window_y: u8,
    scanline_compare: u8,
}

impl Registers {
    pub const SCANLINE_PER_FRAME: u8 = 154;

    #[inline]
    pub fn control_register(&self) -> &ControlRegister {
        &self.control
    }

    #[inline]
    pub fn interrupt_sources(&self) -> &InterruptSources {
        &self.interrupts
    }

    #[inline]
    pub fn background_palette(&self) -> &PaletteRegister {
        &self.background_palette
    }

    #[inline]
    pub fn sprite_palettes(&self) -> &(PaletteRegister, PaletteRegister) {
        &self.sprite_palettes
    }

    #[inline]
    pub fn scroll_x(&self) -> u8 {
        self.scroll_x
    }

    #[inline]
    pub fn scroll_y(&self) -> u8 {
        self.scroll_y
    }

    #[inline]
    pub fn window_x(&self) -> u8 {
        self.window_x
    }

    #[inline]
    pub fn window_y(&self) -> u8 {
        self.window_y
    }

    #[inline]
    pub fn scanline(&self) -> u8 {
        self.scanline
    }

    #[inline]
    pub fn increment_scanline(&mut self) {
        self.scanline += 1;

        if self.scanline == Registers::SCANLINE_PER_FRAME {
            self.scanline = 0;
        }
    }
}

impl Default for Registers {
    fn default() -> Self {
        Self {
            mode: Mode::OamFetch,
            control: ControlRegister::default(),
            interrupts: InterruptSources::default(),
            background_palette: PaletteRegister::default(),
            sprite_palettes: (PaletteRegister::default(), PaletteRegister::default()),
            scanline: 0,
            scroll_x: 0,
            scroll_y: 0,
            window_x: 0,
            window_y: 0,
            scanline_compare: 0,
        }
    }
}

impl Memory for Registers {
    fn read(&self, address: u16) -> u8 {
        match address {
            address::LCDC => self.control.flags,
            address::STAT => {
                let mut value = self.interrupts.get_register_value();

                if self.scanline == self.scanline_compare {
                    value |= stat_flags::LYC_COMPARE;
                }

                let mode = match self.mode {
                    Mode::HBlank => 0,
                    Mode::VBlank => 1,
                    Mode::OamFetch => 2,
                    Mode::Draw => 3,
                };

                value | mode
            }
            address::SCY => self.scroll_y,
            address::SCX => self.scroll_x,
            address::LY => self.scanline,
            address::LYC => self.scanline_compare,
            address::BGP => self.background_palette.get_register_value(),
            address::OBP0 => self.sprite_palettes.0.get_register_value(),
            address::OBP1 => self.sprite_palettes.1.get_register_value(),
            address::WY => self.window_y,
            address::WX => self.window_x,
            _ => unreachable!(),
        }
    }

    fn write(&mut self, address: u16, value: u8) {
        match address {
            address::LCDC => self.control.flags = value,
            address::STAT => self.interrupts.from_register_value(value),
            address::SCY => self.scroll_y = value,
            address::SCX => self.scroll_x = value,
            address::LYC => self.scanline_compare = value,
            address::BGP => self.background_palette.from_register_value(value),
            address::OBP0 => self.sprite_palettes.0.from_register_value(value),
            address::OBP1 => self.sprite_palettes.1.from_register_value(value),
            address::WY => self.window_y = value,
            address::WX => self.window_x = value,
            _ => (),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::emulator::{memory::Memory, ppu::Mode};

    use super::{address, stat_flags, InterruptSources, PaletteRegister, Registers};

    const INTERRUPT_MASK: u8 = stat_flags::LYC_INTERRUPT_SELECT
        | stat_flags::MODE_0_INTERRUPT_SELECT
        | stat_flags::MODE_1_INTERRUPT_SELECT
        | stat_flags::MODE_2_INTERRUPT_SELECT;

    #[test]
    fn test_interrupts_flags() {
        let mut register = InterruptSources::default();

        for value in 0..=0xFFu8 {
            register.from_register_value(value);
            assert_eq!(register.get_register_value(), value & INTERRUPT_MASK)
        }
    }

    #[test]
    fn test_palette_register() {
        let mut register = PaletteRegister::default();

        for value in 0..=0xFFu8 {
            register.from_register_value(value);
            assert_eq!(register.get_register_value(), value);

            dbg!(&register);
        }
    }

    #[test]
    fn test_ly_read_only() {
        let mut register = Registers::default();

        for value in 0..Registers::SCANLINE_PER_FRAME {
            assert_eq!(value, register.read(address::LY));
            register.increment_scanline();
        }

        assert_eq!(register.read(address::LY), 0);

        for value in 0..=0xFFu8 {
            register.write(address::LY, value);
            assert_eq!(register.scanline, 0);
        }
    }

    #[test]
    fn test_lyc_flag() {
        let mut register = Registers::default();

        for scanline in 0..Registers::SCANLINE_PER_FRAME {
            for compare in 0..=0xFFu8 {
                register.write(address::LYC, compare);

                assert_eq!(
                    register.read(address::STAT) & stat_flags::LYC_COMPARE != 0,
                    scanline == compare
                );
            }

            register.increment_scanline();
        }
    }

    #[test]
    fn test_stat_read_only_bits() {
        let mut register = Registers::default();
        register.mode = Mode::HBlank;
        register.write(address::LYC, 0xFF);

        for value in 0..=0xFFu8 {
            register.write(address::STAT, value);
            assert_eq!(register.read(address::STAT), value & INTERRUPT_MASK);
        }
    }
}
