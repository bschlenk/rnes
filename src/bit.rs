#[repr(u8)]
pub enum Bit {
  Zero = 1 << 0,
  One = 1 << 1,
  Two = 1 << 2,
  Three = 1 << 3,
  Four = 1 << 4,
  Five = 1 << 5,
  Six = 1 << 6,
  Seven = 1 << 7,
}

impl Bit {
  pub fn from(num: u8) -> Bit {
    match num {
      0 => Bit::Zero,
      1 => Bit::One,
      2 => Bit::Two,
      3 => Bit::Three,
      4 => Bit::Four,
      5 => Bit::Five,
      6 => Bit::Six,
      7 => Bit::Seven,
      _ => panic!("Bit::from called with invalid argument {}", num),
    }
  }
}

pub fn check_bit(val: u8, bit: Bit) -> bool {
  val & (bit as u8) != 0
}

pub fn make_u16(lo: u8, hi: u8) -> u16 {
  u16::from_le_bytes([lo, hi])
}

pub fn split_u16(val: u16) -> (u8, u8) {
  let [lo, hi] = val.to_le_bytes();
  (lo, hi)
}

pub fn add_signed(val: u16, op: u8) -> u16 {
  val.wrapping_add((op as i8) as u16)
}

pub fn page_crossed(a: u16, b: u16) -> bool {
  a >> 8 != b >> 8
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_checks_the_correct_bit() {
    assert_eq!(true, check_bit(0b0100_0000, Bit::Six));
  }

  #[test]
  fn it_can_make_a_u16() {
    let lo = 0xCD;
    let hi = 0xAB;

    assert_eq!(0xABCD, make_u16(lo, hi));
  }

  #[test]
  fn it_can_split_a_u16() {
    let (lo, hi) = split_u16(0xABCD);

    assert_eq!(0xCD, lo);
    assert_eq!(0xAB, hi);
  }

  #[test]
  fn it_can_add_signed() {
    let x: u16 = 123;
    let y: u8 = 0xfd; // -3

    assert_eq!(add_signed(x, y), 120);
  }

  #[test]
  fn it_can_detect_page_crossed() {
    assert!(!page_crossed(0x00ab, 0x00cd), "page not crossed");
    assert!(page_crossed(0x00ff, 0x0100), "page crossed");
  }
}
