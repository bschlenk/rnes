mod bit;
mod bus;
mod cpu;
mod opcodes;
mod rom;
mod status;

use bus::*;
use cpu::Cpu;

use std::fs;

#[macro_use]
extern crate lazy_static;

fn main() {
  let mut bus = match fs::read("tests/6502_functional_test.bin") {
    Ok(bus) => bus,
    _ => panic!("bad file?"),
  };

  let mut cpu = Cpu::new(&mut bus);
  cpu.reset();
  cpu.pc = 0x0400;

  cpu.process();
}
