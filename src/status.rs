use crate::bit::Bit;

bitflags! {
  #[derive(Default)]
  pub struct Status: u8 {
    /**
     * The carry flag is set if the last operation caused an overflow from bit 7
     * of the result or an underflow from bit 0. This condition is set during
     * arithmetic, comparison and during logical shifts. It can be explicitly set
     * using the 'Set Carry Flag' (SEC) instruction and cleared with
     * 'Clear Carry Flag' (CLC).
     */
    const CARRY = Bit::Zero as u8;

    /**
     * The zero flag is set if the result of the last operation as was zero.
     */
    const ZERO = Bit::One as u8;

    /**
     * The interrupt disable flag is set if the program has executed a
     * 'Set Interrupt Disable' (SEI) instruction. While this flag is set the
     * processor will not respond to interrupts from devices until it is cleared
     * by a 'Clear Interrupt Disable' (CLI) instruction.
     */
    const INTERRUPT = Bit::Two as u8;

    /**
     * The NES allows this flag to be set, but essentially ignores it.
     */
    const DECIMAL = Bit::Three as u8;

    /**
     * The break command bit is set when a BRK instruction has been executed and
     * an interrupt has been generated to process it.
     */
    const BREAK = Bit::Four as u8;

    /**
     * Set when pulling P from the stack.
     * http://wiki.nesdev.com/w/index.php/Status_flags#The_B_flag
     */
    const BREAK2 = Bit::Five as u8;

    /**
     * Overflow
     *
     * The overflow flag is set during arithmetic operations if the result has
     * yielded an invalid 2's complement result (e.g. adding to positive numbers
     * and ending up with a negative result: 64 + 64 => -128). It is determined by
     * looking at the carry between bits 6 and 7 and between bit 7 and the carry
     * flag.
     */
    const OVERFLOW = Bit::Six as u8;

    /**
     * Negative
     *
     * The negative flag is set if the result of the last operation had bit 7 set
     * to a one.
     */
    const NEGATIVE = Bit::Seven as u8;
  }
}

impl Status {
  pub fn reset(&mut self) {
    self.bits = (Status::INTERRUPT | Status::BREAK2).bits;
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_resets_to_i_and_b2() {
    let mut s = Status::default();
    s.reset();
    assert_eq!(s, Status::INTERRUPT | Status::BREAK2);
  }

  #[test]
  fn it_can_set_a_bit() {
    let mut s = Status::default();
    s.set(Status::NEGATIVE, true);

    assert_eq!(0b1000_0000, s.bits());
    assert_eq!(true, s.contains(Status::NEGATIVE));
  }
}
