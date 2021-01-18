use crate::bit::{make_u16, split_u16};
use crate::rom::Rom;

pub trait Bus {
  fn read(&self, addr: u16) -> u8;

  fn read_u16(&self, addr: u16) -> u16 {
    let lo = self.read(addr);
    let hi = self.read(addr.wrapping_add(1));
    make_u16(lo, hi)
  }

  /**
   * Read a u16 from the given 8-bit address. If the given addr is 0xff, the
   * high byte is wrapped and read from 0x00.
   */
  fn wrapping_read_u16(&self, addr: u8) -> u16 {
    let lo = self.read(addr as u16);
    let hi = self.read(addr.wrapping_add(1) as u16);
    make_u16(lo, hi)
  }

  fn write(&mut self, addr: u16, val: u8);

  fn write_u16(&mut self, addr: u16, val: u16) {
    let (lo, hi) = split_u16(val);
    self.write(addr, lo);
    self.write(addr + 1, hi);
  }
}

impl Bus for Vec<u8> {
  fn read(&self, addr: u16) -> u8 {
    self[addr as usize]
  }

  fn write(&mut self, addr: u16, val: u8) {
    self[addr as usize] = val;
  }
}

pub struct DataBus {
  ram: [u8; 2048],
  rom: Rom,
}

impl DataBus {
  pub fn new(rom: Rom) -> Self {
    Self {
      ram: [0; 2048],
      rom,
    }
  }

  fn read_prg_rom(&self, mut addr: u16) -> u8 {
    addr -= 0x8000;
    if self.rom.prg_rom.len() == 0x4000 && addr >= 0x4000 {
      // mirror if needed
      addr = addr % 0x4000;
    }
    self.rom.prg_rom[addr as usize]
  }
}

impl Bus for DataBus {
  fn read(&self, addr: u16) -> u8 {
    match addr {
      0..=0x1fff => {
        let mirrored = addr & 0b111_1111_1111;
        self.ram[mirrored as usize]
      }
      0x2000..=0x3fff => {
        let _mirrored = addr & 0b10_0000_0000_0111;
        todo!("PPU not implemented")
      }
      0x8000..=0xFFFF => self.read_prg_rom(addr),
      _ => {
        println!("Ignoring memory access for {}", addr);
        0
      }
    }
  }

  fn write(&mut self, addr: u16, val: u8) {
    match addr {
      0..=0x1fff => {
        let mirrored = addr & 0b111_1111_1111;
        self.ram[mirrored as usize] = val;
      }
      0x2000..=0x3fff => {
        let _mirrored = addr & 0b10_0000_0000_0111;
        todo!("PPU not implemented")
      }
      0x8000..=0xFFFF => panic!("Attempted to write to ROM"),
      _ => {
        println!("Ignoring memory write for {}", addr);
      }
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_read_u16() {
    let bus = vec![0xCD, 0xAB];
    assert_eq!(0xABCD, bus.read_u16(0));
  }

  #[test]
  fn test_write_u16() {
    let mut bus = vec![0; 2];
    bus.write_u16(0, 0xABCD);

    assert_eq!(bus[0], 0xCD);
    assert_eq!(bus[1], 0xAB);
  }

  #[test]
  fn test_wrapping_read_u16() {
    let mut bus = vec![0; 0x100];
    bus[0x00] = 0xab;
    bus[0xff] = 0xcd;

    assert_eq!(bus.wrapping_read_u16(0xff), 0xabcd)
  }

  #[test]
  fn test_data_bus_mirroring() {
    let rom = Rom::default();
    let mut bus = DataBus::new(rom);

    bus.write(0, 0xab);

    assert_eq!(bus.read(0x800), 0xab);
  }
}
