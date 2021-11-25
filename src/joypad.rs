use crate::bit::{check_bit, Bit};

#[derive(Debug)]
pub struct Joypad {
  strobe: bool,
  index: u8,
  buttons: JoypadButton,
}

impl Joypad {
  pub fn new() -> Self {
    Self {
      strobe: false,
      index: 0,
      buttons: JoypadButton::default(),
    }
  }

  pub fn set_button(&mut self, btn: JoypadButton, status: bool) {
    self.buttons.set(btn, status);
  }

  pub fn write(&mut self, val: u8) {
    self.strobe = check_bit(val, Bit::Zero);
    if self.strobe {
      self.index = 0;
    }
  }

  pub fn read(&mut self) -> u8 {
    if self.index > 7 {
      return 1;
    }
    let response = check_bit(self.buttons.bits, Bit::from(self.index)) as u8;
    if !self.strobe {
      self.index += 1;
    }
    response
  }
}

bitflags! {
  #[derive(Default)]
  pub struct JoypadButton: u8 {
    const A = Bit::Zero as u8;
    const B = Bit::One as u8;
    const SELECT = Bit::Two as u8;
    const START = Bit::Three as u8;
    const UP = Bit::Four as u8;
    const DOWN = Bit::Five as u8;
    const LEFT = Bit::Six as u8;
    const RIGHT = Bit::Seven as u8;
  }
}
