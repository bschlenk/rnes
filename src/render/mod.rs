pub mod frame;
pub mod palette;

use crate::bit::{check_bit, Bit};
use crate::ppu::Ppu;
use frame::Frame;
use palette::SYSTEM_PALETTE;

pub fn render(ppu: &Ppu, frame: &mut Frame) {
  let bank = ppu.ctrl.background_pattern_addr();

  for i in 0..0x03c0 {
    // just for now, lets use the first nametable
    let tile = ppu.vram[i] as u16;
    let tile_x = i % 32;
    let tile_y = i / 32;
    let tile = &ppu.chr_rom[(bank + tile * 16) as usize..(bank + tile * 16 + 16) as usize];
    let palette = ppu.bg_pallette(tile_x, tile_y);

    for y in 0..8 {
      let mut upper = tile[y];
      let mut lower = tile[y + 8];

      for x in (0..8).rev() {
        let value = (1 & lower) << 1 | (1 & upper);
        upper = upper >> 1;
        lower = lower >> 1;
        let rgb = match value {
          0 => SYSTEM_PALETTE[ppu.palette_table[0] as usize],
          1 => SYSTEM_PALETTE[palette[1] as usize],
          2 => SYSTEM_PALETTE[palette[2] as usize],
          3 => SYSTEM_PALETTE[palette[3] as usize],
          _ => unreachable!(),
        };
        frame.set_pixel(tile_x * 8 + x, tile_y * 8 + y, rgb)
      }
    }
  }

  // draw sprites
  for i in (0..ppu.oam_data.len()).step_by(4).rev() {
    let tile_idx = ppu.oam_data[i + 1] as u16;
    let tile_x = ppu.oam_data[i + 3] as usize;
    let tile_y = ppu.oam_data[i] as usize;
    let attrs = ppu.oam_data[i + 2];

    let flip_vertical = check_bit(attrs, Bit::Seven);
    let flip_horizontal = check_bit(attrs, Bit::Six);
    let pallette_idx = attrs & 0b11;
    let sprite_palette = ppu.sprite_palette(pallette_idx);

    let bank: u16 = ppu.ctrl.sprite_pattern_addr();

    let tile = &ppu.chr_rom[(bank + tile_idx * 16) as usize..(bank + tile_idx * 16 + 16) as usize];

    for y in 0..8 {
      let mut upper = tile[y];
      let mut lower = tile[y + 8];
      for x in (0..8).rev() {
        let value = (1 & lower) << 1 | (1 & upper);
        upper = upper >> 1;
        lower = lower >> 1;
        let rgb = match value {
          0 => continue, // skip coloring the pixel
          1 => SYSTEM_PALETTE[sprite_palette[1] as usize],
          2 => SYSTEM_PALETTE[sprite_palette[2] as usize],
          3 => SYSTEM_PALETTE[sprite_palette[3] as usize],
          _ => unreachable!(),
        };
        match (flip_horizontal, flip_vertical) {
          (false, false) => frame.set_pixel(tile_x + x, tile_y + y, rgb),
          (true, false) => frame.set_pixel(tile_x + 7 - x, tile_y + y, rgb),
          (false, true) => frame.set_pixel(tile_x + x, tile_y + 7 - y, rgb),
          (true, true) => frame.set_pixel(tile_x + 7 - x, tile_y + 7 - y, rgb),
        }
      }
    }
  }
}
