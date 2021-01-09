use crate::bit::Bit;

pub struct Status {
  pub bits: u8,
}

impl Status {
  pub fn new() -> Status {
    Status { bits: 0 }
  }

  /**
   * Negative
   *
   * The negative flag is set if the result of the last operation had bit 7 set
   * to a one.
   */
  pub fn get_n(&self) -> bool {
    self.get_bit(Bit::Seven)
  }

  pub fn set_n(&mut self, val: bool) {
    self.set_bit(Bit::Seven, val);
  }

  /**
   * Overflow
   *
   * The overflow flag is set during arithmetic operations if the result has
   * yielded an invalid 2's complement result (e.g. adding to positive numbers
   * and ending up with a negative result: 64 + 64 => -128). It is determined by
   * looking at the carry between bits 6 and 7 and between bit 7 and the carry
   * flag.
   */
  pub fn get_v(&self) -> bool {
    self.get_bit(Bit::Six)
  }

  pub fn set_v(&mut self, val: bool) {
    self.set_bit(Bit::Six, val);
  }

  /**
   * Break
   *
   * The break command bit is set when a BRK instruction has been executed and
   * an interrupt has been generated to process it.
   */
  pub fn get_b(&self) -> bool {
    self.get_bit(Bit::Four)
  }

  pub fn set_b(&mut self, val: bool) {
    self.set_bit(Bit::Four, val);
  }

  /**
   * Decimal (use BCD for arithmetics)
   *
   * While the decimal mode flag is set the processor will obey the rules of
   * Binary Coded Decimal (BCD) arithmetic during addition and subtraction.
   * The flag can be explicitly set using 'Set Decimal Flag' (SED) and cleared
   * with 'Clear Decimal Flag' (CLD).
   */
  pub fn get_d(&self) -> bool {
    self.get_bit(Bit::Three)
  }

  pub fn set_d(&mut self, val: bool) {
    self.set_bit(Bit::Three, val);
  }

  /**
   * Interrupt (IRQ disable)
   *
   * The interrupt disable flag is set if the program has executed a
   * 'Set Interrupt Disable' (SEI) instruction. While this flag is set the
   * processor will not respond to interrupts from devices until it is cleared
   * by a 'Clear Interrupt Disable' (CLI) instruction.
   */
  pub fn get_i(&self) -> bool {
    self.get_bit(Bit::Two)
  }

  pub fn set_i(&mut self, val: bool) {
    self.set_bit(Bit::Two, val);
  }

  /**
   * Zero
   *
   * The zero flag is set if the result of the last operation as was zero.
   */
  pub fn get_z(&self) -> bool {
    self.get_bit(Bit::One)
  }

  pub fn set_z(&mut self, val: bool) {
    self.set_bit(Bit::One, val);
  }

  /**
   * Carry
   *
   * The carry flag is set if the last operation caused an overflow from bit 7
   * of the result or an underflow from bit 0. This condition is set during
   * arithmetic, comparison and during logical shifts. It can be explicitly set
   * using the 'Set Carry Flag' (SEC) instruction and cleared with
   * 'Clear Carry Flag' (CLC).
   */
  pub fn get_c(&self) -> bool {
    self.get_bit(Bit::Zero)
  }

  pub fn set_c(&mut self, val: bool) {
    self.set_bit(Bit::Zero, val);
  }

  #[inline]
  fn get_bit(&self, bit: Bit) -> bool {
    self.bits & (bit as u8) > 0
  }

  #[inline]
  fn set_bit(&mut self, bit: Bit, val: bool) {
    if val {
      self.bits |= bit as u8;
    } else {
      self.bits &= !(bit as u8);
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_can_set_a_bit() {
    let mut s = Status::new();
    s.set_n(true);

    assert_eq!(0b1000_0000, s.bits);
    assert_eq!(true, s.get_n());
  }
}
