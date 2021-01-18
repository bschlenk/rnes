#[derive(Debug, PartialEq)]
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

struct Ppu {
  pub chr_rom: Vec<u8>,
  pub palette_table: [u8; 32],
  pub vram: [u8; 2048],
  pub oam_data: [u8; 256],

  pub mirroring: Mirroring,
}

impl Ppu {
  pub fn new(chr_rom: Vec<u8>, mirroring: Mirroring) -> Self {
    Self {
      chr_rom,
      palette_table: [0; 32],
      vram: [0; 2048],
      oam_data: [0; 256],
      mirroring,
    }
  }
}
