mod bit;
mod bus;
mod cpu;
mod opcodes;
mod ppu;
mod rom;
mod status;

use bus::*;
use cpu::Cpu;
use rom::Rom;

#[macro_use]
extern crate bitflags;

#[macro_use]
extern crate lazy_static;

fn main() {
  let rom = match Rom::load_from_file("./tests/nestest/nestest.nes") {
    Ok(rom) => rom,
    Err(err) => panic!("could not load rom: {}", err),
  };

  let mut bus = DataBus::new(rom);
  let mut cpu = Cpu::new(&mut bus);
  cpu.reset();
  cpu.pc = 0xc000;

  cpu.process();
}
