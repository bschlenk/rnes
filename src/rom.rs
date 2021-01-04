use crate::bit::{check_bit, Bit};
use std::fs::File;
use std::io::{self, prelude::*};

// N,E,S,EOL
const MAGIC: [u8; 4] = [0x4E, 0x45, 0x53, 0x1A];

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

pub struct Rom {
  mem: Vec<u8>,
  prg_rom: u8,
  chr_rom: u8,
  mirroring: Mirroring,
  battery_backed_ram: bool,
  trainer: bool,
  mapper: u8,
}

impl Rom {
  pub fn load_from_file(path: &str) -> Result<Self, io::Error> {
    let f = File::open(path)?;
    let header: [u8; 16] = [0; 16];
    f.read_exact(&mut header)?;

    if header[0..4] != MAGIC {
      return Err(io::Error::new(io::ErrorKind::Other, "not an NES file"));
    }

    let prg_rom = header[4];
    let chr_rom = header[5];
    let rom_control_1 = header[6];
    let rom_control_2 = header[7];
    let ram = header[8];

    let vertical_mirroring = check_bit(rom_control_1, Bit::Zero);
    let battery_backed_ram = check_bit(rom_control_1, Bit::One);
    let trainer = check_bit(rom_control_1, Bit::Two);
    let four_screen_mirroring = check_bit(rom_control_1, Bit::Four);

    let mirroring = if four_screen_mirroring {
      Mirroring::FourScreen
    } else if vertical_mirroring {
      Mirroring::Vertical
    } else {
      Mirroring::Horizontal
    };

    let mapper = rom_control_2 | (rom_control_1 >> 4);

    let mem = Vec::new();
    f.read_to_end(&mut mem)?;

    Ok(Rom {
      mem,
      prg_rom,
      chr_rom,
      mirroring,
      battery_backed_ram,
      trainer,
      mapper,
    })
  }
}

pub enum Mirroring {
  Horizontal,
  Vertical,
  FourScreen,
}
