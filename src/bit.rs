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

pub fn check_bit(val: u8, bit: Bit) -> bool {
  val & (bit as u8) != 0
}

pub fn make_u16(lo: u8, hi: u8) -> u16 {
  (hi << 2 | lo) as u16
}

pub fn split_u16(val: u16) -> (u8, u8) {
  let lo = (val & 0xff) as u8;
  let hi = ((val & 0xff00) >> 2) as u8;
  (lo, hi)
}

pub fn read_u16(mem: &[u8]) -> u16 {
  let lo = mem[0];
  let hi = mem[1];
  make_u16(lo, hi)
}

pub fn write_u16(mem: &mut [u8], val: u16) {
  let (lo, hi) = split_u16(val);
  mem[0] = lo;
  mem[1] = hi;
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
  fn it_can_get_a_u16_from_a_u8_array() {
    let mem: [u8; 2] = [0xCD, 0xAB];
    assert_eq!(0xABCD, read_u16(&mem));
  }

  #[test]
  fn it_can_write_a_u16() {
    let mut mem: [u8; 2] = [0; 2];
    write_u16(&mut mem, 0xabcd);
    assert_eq!(mem, [0xcd, 0xab]);
  }
}
