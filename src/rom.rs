use crate::bit::{check_bit, Bit};
use crate::ppu::Mirroring;
use std::fs;

// N,E,S,EOL
const MAGIC: [u8; 4] = [0x4E, 0x45, 0x53, 0x1A];
const PRG_ROM_PAGE_SIZE: usize = 16 * 1024;
const CHR_ROM_PAGE_SIZE: usize = 8 * 1024;
// const PRG_RAM_PAGE_SIZE: usize = 8 * 1024;
const TRAINER_SIZE: usize = 512;

/*

0-3: Constant $4E $45 $53 $1A ("NES" followed by MS-DOS end-of-file)
4: Size of PRG ROM in 16 KB units
5: Size of CHR ROM in 8 KB units (Value 0 means the board uses CHR RAM)
6: Flags 6 - Mapper, mirroring, battery, trainer
7: Flags 7 - Mapper, VS/Playchoice, NES 2.0
8: Flags 8 - PRG-RAM size (rarely used extension)
9: Flags 9 - TV system (rarely used extension)
10: Flags 10 - TV system, PRG-RAM presence (unofficial, rarely used extension)
11-15: Unused padding (should be filled with zero, but some rippers put their name across bytes 7-15)

*/

#[derive(Default)]
pub struct Rom {
  pub prg_rom: Vec<u8>,
  pub chr_rom: Vec<u8>,
  pub mirroring: Mirroring,
  pub battery_backed_ram: bool,
  pub trainer: Option<Vec<u8>>,
  pub mapper: u8,
}

impl Rom {
  pub fn load_from_file(path: &str) -> Result<Self, String> {
    let data = match fs::read(path) {
      Ok(data) => data,
      Err(error) => panic!("{}", error),
    };

    Self::new(data)
  }

  pub fn new(data: Vec<u8>) -> Result<Self, String> {
    if &data[0..4] != MAGIC {
      return Err("not an iNES file".to_string());
    }

    let rom_control_1 = data[6];
    let rom_control_2 = data[7];

    let ines_ver = (rom_control_2 >> 2) & 0b11;
    if ines_ver != 0 {
      return Err("iNES 2.0 not supported".to_string());
    }

    let vertical_mirroring = check_bit(rom_control_1, Bit::Zero);
    let battery_backed_ram = check_bit(rom_control_1, Bit::One);
    let has_trainer = check_bit(rom_control_1, Bit::Two);
    let four_screen_mirroring = check_bit(rom_control_1, Bit::Three);

    let mirroring = match (four_screen_mirroring, vertical_mirroring) {
      (true, _) => Mirroring::FourScreen,
      (false, true) => Mirroring::Vertical,
      (false, false) => Mirroring::Horizontal,
    };

    let mapper = (rom_control_2 & 0b1111_0000) | (rom_control_1 >> 4);

    let mut trainer = None;
    if has_trainer {
      trainer = Some(data[16..(16 + TRAINER_SIZE)].to_vec());
    }

    let prg_rom_len = data[4] as usize * PRG_ROM_PAGE_SIZE;
    let chr_rom_len = data[5] as usize * CHR_ROM_PAGE_SIZE;
    // let prg_ram_len = data[8] as usize * PRG_RAM_PAGE_SIZE;

    let prg_rom_start = 16 + if has_trainer { TRAINER_SIZE } else { 0 };
    let chr_rom_start = prg_rom_start + prg_rom_len;

    let prg_rom = data[prg_rom_start..(prg_rom_start + prg_rom_len)].to_vec();
    let chr_rom = data[chr_rom_start..(chr_rom_start + chr_rom_len)].to_vec();

    Ok(Rom {
      prg_rom,
      chr_rom,
      mirroring,
      battery_backed_ram,
      trainer,
      mapper,
    })
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_load_file() {
    let res = Rom::load_from_file("./roms/Balloon Fight (E).nes");
    assert!(!res.is_err(), "the file should have loaded");
    let rom = res.unwrap();
    assert_eq!(rom.mirroring, Mirroring::Horizontal);
  }
}
