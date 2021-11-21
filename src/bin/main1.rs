use rnes::{bus::*, cpu::Cpu, rom::Rom};

fn main() {
  let rom = match Rom::load_from_file("./tests/nestest/nestest.nes") {
    Ok(rom) => rom,
    Err(err) => panic!("could not load rom: {}", err),
  };

  let mut bus = DataBus::new(rom, |_, _| {});
  let mut cpu = Cpu::new(&mut bus);
  cpu.reset();
  cpu.pc = 0xc000;

  cpu.run();
}
