mod bit;
mod bus;
mod cpu;
mod joypad;
mod opcodes;
mod ppu;
mod render;
mod rom;
mod status;

use std::collections::HashMap;

use bus::*;
use cpu::Cpu;
use joypad::{Joypad, JoypadButton};
use ppu::Ppu;
use render::frame::Frame;
use render::palette::SYSTEM_PALETTE;
use rom::Rom;

use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;

#[macro_use]
extern crate bitflags;

#[macro_use]
extern crate lazy_static;

fn show_tile(chr_rom: &Vec<u8>, bank: usize, tile_n: usize) -> Frame {
  assert!(bank <= 1);

  let mut frame = Frame::new();
  let bank = (bank * 0x1000) as usize;

  let tile = &chr_rom[(bank + tile_n * 16)..=(bank + tile_n * 16 + 15)];

  for y in 0..=7 {
    let mut upper = tile[y];
    let mut lower = tile[y + 8];

    for x in (0..=7).rev() {
      let value = (1 & upper) << 1 | (1 & lower);
      upper = upper >> 1;
      lower = lower >> 1;
      let rgb = match value {
        0 => SYSTEM_PALETTE[0x01],
        1 => SYSTEM_PALETTE[0x23],
        2 => SYSTEM_PALETTE[0x27],
        3 => SYSTEM_PALETTE[0x30],
        _ => panic!("can't be"),
      };
      frame.set_pixel(x, y, rgb)
    }
  }

  frame
}

fn show_tile_bank(chr_rom: &Vec<u8>, bank: usize) -> Frame {
  assert!(bank <= 1);

  let mut frame = Frame::new();
  let mut tile_y = 0;
  let mut tile_x = 0;
  let bank = (bank * 0x1000) as usize;

  for tile_n in 0..255 {
    if tile_n != 0 && tile_n % 20 == 0 {
      tile_y += 10;
      tile_x = 0;
    }
    let tile = &chr_rom[(bank + tile_n * 16)..=(bank + tile_n * 16 + 15)];

    for y in 0..=7 {
      let mut upper = tile[y];
      let mut lower = tile[y + 8];

      for x in (0..=7).rev() {
        let value = (1 & upper) << 1 | (1 & lower);
        upper = upper >> 1;
        lower = lower >> 1;
        let rgb = match value {
          0 => SYSTEM_PALETTE[0x01],
          1 => SYSTEM_PALETTE[0x23],
          2 => SYSTEM_PALETTE[0x27],
          3 => SYSTEM_PALETTE[0x30],
          _ => unreachable!(),
        };
        frame.set_pixel(tile_x + x, tile_y + y, rgb)
      }
    }

    tile_x += 10;
  }
  frame
}

fn main() {
  // init sdl2
  let sdl_context = sdl2::init().unwrap();
  let video_subsystem = sdl_context.video().unwrap();
  let window = video_subsystem
    .window("Tile viewer", (256.0 * 3.0) as u32, (240.0 * 3.0) as u32)
    .position_centered()
    .build()
    .unwrap();

  let mut canvas = window.into_canvas().present_vsync().build().unwrap();
  let mut event_pump = sdl_context.event_pump().unwrap();
  canvas.set_scale(3.0, 3.0).unwrap();

  let creator = canvas.texture_creator();
  let mut texture = creator
    .create_texture_target(PixelFormatEnum::RGB24, 256, 240)
    .unwrap();

  let rom = Rom::load_from_file("./roms/Pac-Man (U) [!].nes").unwrap();

  let mut key_map = HashMap::with_capacity(8);
  key_map.insert(Keycode::Down, JoypadButton::DOWN);
  key_map.insert(Keycode::Up, JoypadButton::UP);
  key_map.insert(Keycode::Right, JoypadButton::RIGHT);
  key_map.insert(Keycode::Left, JoypadButton::LEFT);
  key_map.insert(Keycode::Space, JoypadButton::SELECT);
  key_map.insert(Keycode::Return, JoypadButton::START);
  key_map.insert(Keycode::A, JoypadButton::A);
  key_map.insert(Keycode::S, JoypadButton::B);

  let mut frame = Frame::new();

  // the game cycle
  let mut bus = DataBus::new(rom, move |ppu: &Ppu, joypad: &mut Joypad| {
    render::render(ppu, &mut frame);
    texture.update(None, &frame.data, 256 * 3).unwrap();

    canvas.copy(&texture, None, None).unwrap();

    canvas.present();
    for event in event_pump.poll_iter() {
      match event {
        Event::Quit { .. }
        | Event::KeyDown {
          keycode: Some(Keycode::Escape),
          ..
        } => std::process::exit(0),

        Event::KeyDown { keycode, .. } => {
          if let Some(key) = key_map.get(&keycode.unwrap_or(Keycode::Ampersand)) {
            joypad.set_button(*key, true);
          }
        }

        Event::KeyUp { keycode, .. } => {
          if let Some(key) = key_map.get(&keycode.unwrap_or(Keycode::Ampersand)) {
            joypad.set_button(*key, false);
          }
        }
        _ => { /* do nothing */ }
      }
    }
  });

  let mut cpu = Cpu::new(&mut bus);

  cpu.reset();
  cpu.run();
}
