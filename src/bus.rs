use crate::bit::{make_u16, split_u16};
use crate::joypad::Joypad;
use crate::ppu::Ppu;
use crate::rom::Rom;

pub trait Bus {
  fn read(&mut self, addr: u16) -> u8;

  fn read_u16(&mut self, addr: u16) -> u16 {
    let lo = self.read(addr);
    let hi = self.read(addr.wrapping_add(1));
    make_u16(lo, hi)
  }

  /**
   * Read a u16 from the given 8-bit address. If the given addr is 0xff, the
   * high byte is wrapped and read from 0x00.
   */
  fn wrapping_read_u16(&mut self, addr: u8) -> u16 {
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

  fn tick(&mut self, cpu_cycles: usize);

  fn poll_nmi_status(&mut self) -> Option<u8>;
}

impl Bus for Vec<u8> {
  fn read(&mut self, addr: u16) -> u8 {
    self[addr as usize]
  }

  fn write(&mut self, addr: u16, val: u8) {
    self[addr as usize] = val;
  }

  fn tick(&mut self, _cpu_cycles: usize) {
    // pass
  }

  fn poll_nmi_status(&mut self) -> Option<u8> {
    None
  }
}

pub struct DataBus<'a> {
  ram: [u8; 2048],
  prg_rom: Vec<u8>,
  ppu: Ppu,
  joypad: Joypad,
  cpu_cycles: usize,
  gameloop_callback: Box<dyn FnMut(&Ppu, &mut Joypad) + 'a>,
}

impl<'a> DataBus<'a> {
  pub fn new<F>(rom: Rom, gameloop_callback: F) -> DataBus<'a>
  where
    F: FnMut(&Ppu, &mut Joypad) + 'a,
  {
    let ppu = Ppu::new(rom.chr_rom, rom.mirroring);
    let joypad = Joypad::new();

    Self {
      ram: [0; 2048],
      prg_rom: rom.prg_rom,
      ppu,
      joypad,
      cpu_cycles: 0,
      gameloop_callback: Box::from(gameloop_callback),
    }
  }

  fn read_prg_rom(&self, mut addr: u16) -> u8 {
    addr -= 0x8000;
    if self.prg_rom.len() == 0x4000 && addr >= 0x4000 {
      // mirror if needed
      addr = addr % 0x4000;
    }
    self.prg_rom[addr as usize]
  }
}

impl<'a> Bus for DataBus<'a> {
  fn read(&mut self, addr: u16) -> u8 {
    match addr {
      0..=0x1fff => {
        let mirrored = addr & 0b111_1111_1111;
        self.ram[mirrored as usize]
      }
      0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 | 0x4014 => {
        // panic!("Attempt to read from write-only PPU address {:x}", addr)
        0 // debugger tries to read from here for some reason
      }
      0x2002 => self.ppu.read_status(),
      0x2004 => self.ppu.read_oam_data(),
      0x2007 => self.ppu.read_data(),
      0x4016 => self.joypad.read(),
      0x2008..=0x7fff => {
        // ppu memory mirrored
        let mirrored = addr & 0b10_0000_0000_0111;
        self.read(mirrored)
      }
      0x8000..=0xffff => self.read_prg_rom(addr),
    }
  }

  fn write(&mut self, addr: u16, val: u8) {
    match addr {
      0..=0x1fff => {
        let mirrored = addr & 0b111_1111_1111;
        self.ram[mirrored as usize] = val;
      }
      0x2000 => self.ppu.write_ctrl(val),
      0x2001 => self.ppu.write_ctrl_2(val),
      0x2002 => panic!("attempt to write ppu status reg"),
      0x2003 => self.ppu.write_oam_addr(val),
      0x2004 => self.ppu.write_oam_data(val),
      0x2005 => self.ppu.write_scroll(val),
      0x2006 => self.ppu.write_ppu_addr(val),
      0x2007 => self.ppu.write_data(val),
      0x4016 => self.joypad.write(val),
      0x2008..=0x7fff => {
        let mirrored = addr & 0b10_0000_0000_0111;
        self.write(mirrored, val);
      }
      0x8000..=0xffff => panic!("Attempted to write to ROM"),
    }
  }

  fn tick(&mut self, cpu_cycles: usize) {
    self.cpu_cycles += cpu_cycles;

    let nmi_before = self.ppu.nmi_interrupt.is_some();
    self.ppu.tick(cpu_cycles * 3);
    let nmi_after = self.ppu.nmi_interrupt.is_some();

    if !nmi_before && nmi_after {
      (self.gameloop_callback)(&self.ppu, &mut self.joypad);
    }
  }

  fn poll_nmi_status(&mut self) -> Option<u8> {
    self.ppu.nmi_interrupt.take()
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_read_u16() {
    let mut bus = vec![0xCD, 0xAB];
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
    let mut bus = DataBus::new(rom, Box::from(|_: &Ppu| {}));

    bus.write(0, 0xab);

    assert_eq!(bus.read(0x800), 0xab);
  }
}
