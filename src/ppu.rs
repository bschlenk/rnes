use crate::bit::{make_u16, Bit};

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Mirroring {
  Horizontal,
  Vertical,
  FourScreen,
}

impl Default for Mirroring {
  fn default() -> Self {
    Self::Horizontal
  }
}

pub struct Ppu {
  pub chr_rom: Vec<u8>,
  pub mirroring: Mirroring,
  pub vram: [u8; 2048],
  pub nmi_interrupt: Option<u8>,

  pub scanline: u16,
  pub cycles: usize,

  status: StatusReg,
  pub ctrl: CtrlReg,
  ctrl2: CtrlReg2,
  addr: AddrReg,
  scroll: ScrollReg,

  oam_addr: u8,
  pub oam_data: [u8; 256],
  pub palette_table: [u8; 32],

  buff: u8,
}

impl Ppu {
  pub fn new(chr_rom: Vec<u8>, mirroring: Mirroring) -> Self {
    Self {
      chr_rom,
      mirroring,
      vram: [0; 2048],
      nmi_interrupt: None,

      scanline: 0,
      cycles: 0,

      status: StatusReg::empty(),
      ctrl: CtrlReg::empty(),
      ctrl2: CtrlReg2::empty(),
      addr: AddrReg::default(),
      scroll: ScrollReg::default(),

      oam_addr: 0,
      oam_data: [0; 256],
      palette_table: [0; 32],

      buff: 0,
    }
  }

  pub fn tick(&mut self, ppu_cycles: usize) -> bool {
    self.cycles += ppu_cycles;

    // http://wiki.nesdev.com/w/index.php/PPU_rendering#Line-by-line_timing
    if self.cycles > 341 {
      self.cycles = self.cycles - 341;
      self.scanline += 1;

      println!("ppu scanline: {}", self.scanline);

      if self.scanline == 241 {
        println!("ppu in vblank!!!");
        self.status.set_vblank(true);
        self.status.remove(StatusReg::HIT);
        if self.ctrl.generate_vblank_nmi() {
          self.nmi_interrupt = Some(1);
        }
      }

      if self.scanline >= 262 {
        self.scanline = 0;
        self.nmi_interrupt = None;
        self.status.remove(StatusReg::HIT);
        self.status.set_vblank(false);
        return true;
      }
    }

    false
  }

  pub fn read_status(&mut self) -> u8 {
    let status = self.status.bits();
    self.status.remove(StatusReg::VBLANK);
    self.addr.reset();
    self.scroll.reset();
    status
  }

  pub fn write_ctrl(&mut self, val: u8) {
    let vblank_old = self.ctrl.generate_vblank_nmi();
    self.ctrl.update(val);
    if !vblank_old && self.ctrl.generate_vblank_nmi() && self.status.contains(StatusReg::VBLANK) {
      self.nmi_interrupt = Some(1);
    }
  }

  pub fn write_ctrl_2(&mut self, val: u8) {
    self.ctrl2.update(val);
  }

  pub fn write_ppu_addr(&mut self, addr: u8) {
    self.addr.update(addr);
  }

  pub fn write_scroll(&mut self, val: u8) {
    self.scroll.write(val);
  }

  pub fn read_data(&mut self) -> u8 {
    let addr = self.addr.get();
    self.inc_vram();

    match addr {
      0..=0x1fff => self.buffer(self.chr_rom[addr as usize]),
      0x2000..=0x2fff => self.buffer(self.vram[self.mirror_vram_addr(addr) as usize]),
      0x3000..=0x3eff => panic!(
        "addr space 0x3000..0x3eff is not expected to be used, requested = {}",
        addr
      ),
      // Addresses $3F10/$3F14/$3F18/$3F1C are mirrors of $3F00/$3F04/$3F08/$3F0C
      0x3f10 | 0x3f14 | 0x3f18 | 0x3f1c => {
        let add_mirror = addr - 0x10;
        self.palette_table[(add_mirror - 0x3f00) as usize]
      }
      0x3f00..=0x3fff => self.palette_table[(addr - 0x3f00) as usize],
      _ => unreachable!(),
    }
  }

  pub fn write_data(&mut self, val: u8) {
    let addr = self.addr.get();
    self.inc_vram();

    match addr {
      0..=0x1fff => self.chr_rom[addr as usize] = val,
      0x2000..=0x2fff => self.vram[self.mirror_vram_addr(addr) as usize] = val,
      0x3000..=0x3eff => panic!(
        "addr space 0x3000..0x3eff is not expected to be used, requested = {}",
        addr
      ),
      // Addresses $3F10/$3F14/$3F18/$3F1C are mirrors of $3F00/$3F04/$3F08/$3F0C
      0x3f10 | 0x3f14 | 0x3f18 | 0x3f1c => {
        let add_mirror = addr - 0x10;
        self.palette_table[(add_mirror - 0x3f00) as usize] = val;
      }
      0x3f00..=0x3fff => self.palette_table[(addr - 0x3f00) as usize] = val,
      _ => unreachable!(),
    }
  }

  pub fn write_oam_addr(&mut self, addr: u8) {
    self.oam_addr = addr;
  }

  pub fn write_oam_data(&mut self, val: u8) {
    self.oam_data[self.oam_addr as usize] = val;
    self.inc_oam_addr();
  }

  pub fn read_oam_data(&self) -> u8 {
    self.oam_data[self.oam_addr as usize]
  }

  pub fn write_oam_dma(&mut self, data: &[u8; 256]) {
    for x in data.iter() {
      self.oam_data[self.oam_addr as usize] = *x;
      self.inc_oam_addr();
    }
  }

  pub fn bg_pallette(&self, tile_column: usize, tile_row: usize) -> [u8; 4] {
    let attr_table_idx = tile_row / 4 * 8 + tile_column / 4;
    let attr_byte = self.vram[0x3c0 + attr_table_idx]; // note: still using hardcoded first nametable

    let pallet_idx = match (tile_column % 4 / 2, tile_row % 4 / 2) {
      (0, 0) => attr_byte & 0b11,
      (1, 0) => (attr_byte >> 2) & 0b11,
      (0, 1) => (attr_byte >> 4) & 0b11,
      (1, 1) => (attr_byte >> 6) & 0b11,
      (_, _) => panic!("should not happen"),
    };

    let pallete_start: usize = 1 + (pallet_idx as usize) * 4;
    [
      self.palette_table[0],
      self.palette_table[pallete_start],
      self.palette_table[pallete_start + 1],
      self.palette_table[pallete_start + 2],
    ]
  }

  pub fn sprite_palette(&self, pallete_idx: u8) -> [u8; 4] {
    let start = 0x11 + (pallete_idx * 4) as usize;
    [
      0,
      self.palette_table[start],
      self.palette_table[start + 1],
      self.palette_table[start + 2],
    ]
  }

  fn inc_vram(&mut self) {
    self.addr.inc(self.ctrl.vram_increment());
  }

  fn inc_oam_addr(&mut self) {
    self.oam_addr = self.oam_addr.wrapping_add(1);
  }

  fn buffer(&mut self, val: u8) -> u8 {
    let result = self.buff;
    self.buff = val;
    result
  }

  fn mirror_vram_addr(&self, addr: u16) -> u16 {
    let mirrored_vram = addr & 0b10111111111111; // mirror down 0x3000-0x3eff to 0x2000 - 0x2eff
    let vram_index = mirrored_vram - 0x2000; // to vram vector
    let name_table = vram_index / 0x400; // to the name table index
    match (&self.mirroring, name_table) {
      (Mirroring::Vertical, 2) | (Mirroring::Vertical, 3) => vram_index - 0x800,
      (Mirroring::Horizontal, 2) => vram_index - 0x400,
      (Mirroring::Horizontal, 1) => vram_index - 0x400,
      (Mirroring::Horizontal, 3) => vram_index - 0x800,
      _ => vram_index,
    }
  }
}

#[derive(Default)]
struct AddrReg {
  // two writes, store the addr, then two reads, then we incrment the pointer?
  addr: u16,
  hi: u8,
  lo_next: bool,
}

impl AddrReg {
  pub fn get(&self) -> u16 {
    self.addr
  }

  pub fn update(&mut self, addr: u8) {
    if self.lo_next {
      self.set(make_u16(addr, self.hi));
    } else {
      self.hi = addr;
    }
    self.lo_next = !self.lo_next;
  }

  pub fn inc(&mut self, inc: u8) {
    self.set(self.addr.wrapping_add(inc as u16))
  }

  pub fn reset(&mut self) {
    self.addr = 0;
    self.lo_next = false;
  }

  fn set(&mut self, addr: u16) {
    self.addr = addr & 0x3fff;
  }
}

#[derive(Default)]
struct ScrollReg {
  pub x: u8,
  pub y: u8,
  x_next: bool,
}

impl ScrollReg {
  pub fn write(&mut self, val: u8) {
    if self.x_next {
      self.x = val
    } else {
      self.y = val
    }
    self.x_next = !self.x_next;
  }

  pub fn reset(&mut self) {
    self.x = 0;
    self.y = 0;
    self.x_next = false;
  }
}

bitflags! {
  pub struct CtrlReg: u8 {
    /// Name Table Address:
    ///
    /// +-----------+-----------+
    /// | 2 ($2800) | 3 ($2C00) |
    /// +-----------+-----------+
    /// | 0 ($2000) | 1 ($2400) |
    /// +-----------+-----------+
    ///
    /// Remember that because of the mirroring there are only 2 real Name Tables,
    /// not 4. Also, PPU will automatically switch to another Name Table when
    /// running off the current Name Table during scroll (see picture above).
    const NAMETABLE1 = Bit::Zero as u8;
    const NAMETABLE2 = Bit::One as u8;
    /// Vertical Write, 1 = PPU memory address increments by 32:
    ///
    /// Name Table, VW=0          Name Table, VW=1
    /// +----------------+        +----------------+
    /// |----> write     |        | | write        |
    /// |                |        | V              |
    const VRAM_INCREMENT = Bit::Two as u8;
    /// Sprite Pattern Table Address, 1 = $1000, 0 = $0000.
    const SPRITE_PATTERN_ADDR = Bit::Three as u8;
    /// Screen Pattern Table Address, 1 = $1000, 0 = $0000.
    const BACKGROUND_PATTERN_ADDR  = Bit::Four as u8;
    /// Sprite Size, 1 = 8x16, 0 = 8x8.
    const SPRITE_SIZE = Bit::Five as u8;
    const UNUSED = Bit::Six as u8;
    /// VBlank Enable, 1 = generate interrupts on VBlank.
    const GENERATE_VBLANK_NMI = Bit::Seven as u8;
  }
}

impl CtrlReg {
  pub fn update(&mut self, data: u8) {
    self.bits = data;
  }

  pub fn vram_increment(&self) -> u8 {
    if self.contains(CtrlReg::VRAM_INCREMENT) {
      32
    } else {
      1
    }
  }

  pub fn nametable_addr(&self) -> u16 {
    match self.bits() & 0b11 {
      0 => 0x2000,
      1 => 0x2400,
      2 => 0x2800,
      3 => 0x2c00,
      _ => unreachable!(),
    }
  }

  pub fn sprite_pattern_addr(&self) -> u16 {
    if self.contains(Self::SPRITE_PATTERN_ADDR) {
      0x1000
    } else {
      0x0000
    }
  }

  pub fn background_pattern_addr(&self) -> u16 {
    if self.contains(Self::BACKGROUND_PATTERN_ADDR) {
      0x1000
    } else {
      0x0000
    }
  }

  pub fn sprite_size(&self) -> u16 {
    if self.contains(Self::SPRITE_SIZE) {
      16
    } else {
      8
    }
  }

  pub fn generate_vblank_nmi(&self) -> bool {
    self.contains(Self::GENERATE_VBLANK_NMI)
  }
}

bitflags! {
  pub struct CtrlReg2: u8 {
    const GREYSCALE = Bit::Zero as u8;
    /// 0 = don't show left 8 columns of the screen.
    const IMAGE_MASK = Bit::One as u8;
    /// 0 = don't show sprites in left 8 columns.
    const SPRITE_MASK = Bit::Two as u8;
    /// Screen Enable, 1 = show picture, 0 = blank screen.
    const SCREEN_ENABLE = Bit::Three as u8;
    /// Sprites Enable, 1 = show sprites, 0 = hide sprites.
    const SPRITES_ENABLE = Bit::Four as u8;
    /// Background Color, 0 = black, 1 = blue, 2 = green, 4 = red.
    const BG_COLOR1 = Bit::Five as u8;
    const BG_COLOR2 = Bit::Six as u8;
    const BG_COLOR3 = Bit::Seven as u8;
  }
}

impl CtrlReg2 {
  pub fn update(&mut self, bits: u8) {
    self.bits = bits;
  }

  pub fn bg_color(&self) -> u8 {
    self.bits() >> 5 & 0b111
  }
}

bitflags! {
  pub struct StatusReg: u8 {
    /// Intended to be set when more than 8 sprites appear on a scanline, but a
    /// hardware bug makes this more complicated.
    const SPRITE_OVERFLOW = Bit::Five as u8;
    /// 1 = Sprite refresh has hit sprite #0.
    /// This flag resets to 0 when screen refresh starts.
    const HIT = Bit::Six as u8;
    /// 1 = PPU is in VBlank state.
    /// This flag resets to 0 when VBlank ends or CPU reads $2002
    const VBLANK = Bit::Seven as u8;
  }
}

impl StatusReg {
  pub fn set_vblank(&mut self, val: bool) {
    self.set(Self::VBLANK, val);
  }
}
