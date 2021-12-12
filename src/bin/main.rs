use std::collections::HashMap;
use std::env;

use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;

use rnes::{
  bus::*,
  cpu::Cpu,
  joypad::{Joypad, JoypadButton},
  ppu::Ppu,
  render,
  render::frame::Frame,
  rom::Rom,
};

fn main() {
  let args: Vec<String> = env::args().collect();

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

  let rom = Rom::load_from_file(&args[1]).unwrap();

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
