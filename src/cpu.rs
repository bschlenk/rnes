use crate::bus::Bus;

use crate::bit::*;
use crate::status::*;

const NMI_VEC: u16 = 0xfffa;
const RESET_VEC: u16 = 0xfffc;
const IRQ_BRK_VEC: u16 = 0xfffe;

const HZ_NTSC: f32 = 1.0 / 1789773.0;
const HZ_PAL: f32 = 1.0 / 1662607.0;

// next pc, cycles
struct OpInfo(u16, u8);

enum AddressMode {
  Immidiate,
  ZeroPage,
  ZeroPageX,
  Absolute,
  AbsoluteX,
  AbsoluteY,
  IndirectX,
  IndirectY,
}

pub struct Cpu<'a> {
  /** The memory bus */
  pub bus: &'a mut (dyn Bus + 'a),

  /**
   * Program Counter
   *
   * The program counter is a 16 bit register which points to the next
   * instruction to be executed. The value of program counter is modified
   * automatically as instructions are executed.
   *
   * The value of the program counter can be modified by executing a jump,
   * a relative branch or a subroutine call to another memory address or
   * by returning from a subroutine or interrupt.
   */
  pub pc: u16,

  /**
   * Stack Pointer
   *
   * The processor supports a 256 byte stack located between $0100 and $01FF.
   * The stack pointer is an 8 bit register and holds the low 8 bits of the next
   * free location on the stack. The location of the stack is fixed and cannot
   * be moved.
   *
   * Pushing bytes to the stack causes the stack pointer to be decremented.
   * Conversely pulling bytes causes it to be incremented.
   *
   * The CPU does not detect if the stack is overflowed by excessive pushing or
   * pulling operations and will most likely result in the program crashing.
   */
  pub s: u8,

  /**
   * Accumulator
   *
   * The 8 bit accumulator is used all arithmetic and logical operations
   * (with the exception of increments and decrements). The contents of the
   * accumulator can be stored and retrieved either from memory or the stack.
   *
   * Most complex operations will need to use the accumulator for arithmetic
   * and efficient optimisation of its use is a key feature of time critical
   * routines.
   */
  pub a: u8,

  /**
   * Index Register X
   *
   * The 8 bit index register is most commonly used to hold counters or offsets
   * for accessing memory. The value of the X register can be loaded and saved
   * in memory, compared with values held in memory or incremented and
   * decremented.
   *
   * The X register has one special function. It can be used to get a copy of
   * the stack pointer or change its value.
   */
  pub x: u8,

  /**
   * Index Register Y
   *
   * The Y register is similar to the X register in that it is available for
   * holding counter or offsets memory access and supports the same set of
   * memory load, save and compare operations as wells as increments and
   * decrements. It has no special functions.
   */
  pub y: u8,

  /**
   * Processor Status
   *
   * As instructions are executed a set of processor flags are set or clear to
   * record the results of the operation. This flags and some additional control
   * flags are held in a special status register. Each flag has a single bit
   * within the register.
   *
   * Instructions exist to test the values of the various bits, to set or clear
   * some of them and to push or pull the entire set to or from the stack.
   */
  pub status: Status,
}

impl<'a> Cpu<'a> {
  fn new(bus: &'a mut dyn Bus) -> Cpu<'a> {
    Cpu {
      bus,
      pc: 0,
      s: 0xff,
      a: 0,
      x: 0,
      y: 0,
      status: Status::new(),
    }
  }

  fn reset(&mut self) {
    self.pc = self.read_u16(RESET_VEC);
  }

  // TODO: remove these methods, just hardcode reading/writing from bus
  fn read(&self, addr: u16) -> u8 {
    self.bus.read(addr)
  }

  fn read_u16(&self, addr: u16) -> u16 {
    self.bus.read_u16(addr)
  }

  fn write(&mut self, addr: u16, val: u8) {
    self.bus.write(addr, val);
  }

  fn write_u16(&mut self, addr: u16, val: u16) {
    self.bus.write_u16(addr, val)
  }

  fn push(&mut self, val: u8) {
    self.write(0x0100 & (self.s as u16), val);
    self.s = self.s.wrapping_sub(1);
  }

  fn pull(&mut self) -> u8 {
    self.s = self.s.wrapping_add(1);
    self.read(0x0100 & (self.s as u16))
  }

  fn push_u16(&mut self, val: u16) {
    let (lo, hi) = split_u16(val);
    self.push(hi);
    self.push(lo);
  }

  fn pull_u16(&mut self) -> u16 {
    let lo = self.pull();
    let hi = self.pull();
    make_u16(lo, hi)
  }

  fn set_z_n_flags(&mut self, val: u8) {
    self.status.set_z(val == 0);
    self.status.set_n(check_bit(val, Bit::Seven));
  }

  // Load / Store Operations

  // first 3 set negative & zero
  fn lda(&mut self, val: u8) {
    self.a = val;
    self.set_z_n_flags(self.a);
  }

  fn ldx(&mut self, mem: &u8) {
    self.x = *mem;
    self.set_z_n_flags(self.x);
  }

  fn ldy(&mut self, mem: &u8) {
    self.y = *mem;
    self.set_z_n_flags(self.y);
  }

  fn sta(&mut self, val: u16) {
    self.write(val, self.a);
  }

  fn stx(&mut self, mem: &mut u8) {
    *mem = self.x;
  }

  fn sty(&mut self, mem: &mut u8) {
    *mem = self.y;
  }

  // Register Transfers

  // all 4 set negative & zero
  fn tax(&mut self) {}
  fn tay(&mut self) {}
  fn txa(&mut self) {}
  fn tya(&mut self) {}

  // Stack Operations

  fn tsx(&mut self) {
    self.x = self.s;
    self.set_z_n_flags(self.x);
  }

  fn txs(&mut self) {
    self.s = self.x;
  }

  fn pha(&mut self) {
    self.push(self.a);
  }

  fn php(&mut self) {
    self.push(self.status.bits);
  }

  fn pla(&mut self) {
    self.a = self.pull();
    self.set_z_n_flags(self.a);
  }

  fn plp(&mut self) {
    self.status.bits = self.pull();
  }

  // Logical

  fn and(&mut self, val: u8) {
    self.a &= val;
    self.set_z_n_flags(self.a);
  }

  fn eor(&mut self, val: u8) {
    self.a ^= val;
    self.set_z_n_flags(self.a);
  }

  fn ora(&mut self, val: u8) {
    self.a |= val;
    self.set_z_n_flags(self.a);
  }

  fn bit(&mut self, mem: &mut u8) {
    self.status.set_z(self.a & *mem == 0);
    self.status.set_v(check_bit(*mem, Bit::Six));
    self.status.set_n(check_bit(*mem, Bit::Seven));
  }

  // Arithmetic

  fn adc(&mut self, val: u8) {
    let initial_acc = self.a;
    self.a += val + (self.status.get_c() as u8);
    self.status.set_c(initial_acc > self.a);
    self
      .status
      .set_v(self.status.get_c() ^ (check_bit(initial_acc, Bit::Seven)));
    self.set_z_n_flags(self.a);
  }

  fn sbc(&mut self, val: u8) {
    let initial_acc = self.a;
    self.a = self.a - val - (1 - self.status.get_c() as u8);
    self.status.set_c(initial_acc < self.a);
    self
      .status
      .set_v(self.status.get_c() as u8 ^ (check_bit(initial_acc, Bit::Seven) as u8) != 0);
    self.set_z_n_flags(self.a);
  }

  fn cmp(&mut self, val: u8) {
    self.status.set_c(self.a >= val);
    self.status.set_z(self.a == val);
    self.status.set_n(self.a > (Bit::Seven as u8));
  }

  // N,Z,C
  fn cpx(&mut self, mem: &u8) {}
  // N,Z,C
  fn cpy(&mut self, mem: &u8) {}

  // Increments & Decrements

  fn inc(&mut self, mem: &mut u8) {
    *mem += 1;
    self.set_z_n_flags(*mem);
  }

  fn inx(&mut self) {
    self.x = self.x.wrapping_add(1);
    self.set_z_n_flags(self.x)
  }

  fn iny(&mut self) {
    self.y += 1;
    self.set_z_n_flags(self.y)
  }

  fn dec(&mut self, mem: &mut u8) {
    *mem -= 1;
    self.set_z_n_flags(*mem);
  }

  fn dex(&mut self) {
    self.x -= 1;
    self.set_z_n_flags(self.x)
  }

  fn dey(&mut self) {
    self.y -= 1;
    self.set_z_n_flags(self.y)
  }

  // Shifts

  fn asl(&mut self, addr: &mut u8) {
    self.status.set_c(check_bit(*addr, Bit::Seven));
    *addr <<= 1;
    self.set_z_n_flags(*addr);
  }

  fn lsr(&mut self, addr: &mut u8) {
    self.status.set_c(check_bit(*addr, Bit::Zero));
    *addr >>= 1;
    self.set_z_n_flags(*addr);
  }

  fn rol(&mut self, addr: &mut u8) {
    // Rotate Left 	N,Z,C
    let carry = self.status.get_c() as u8;
    self.status.set_c(check_bit(*addr, Bit::Seven));
    *addr <<= 1;
    *addr |= carry;
    self.set_z_n_flags(*addr);
  }

  fn ror(&mut self, addr: &mut u8) {
    let carry = self.status.get_c() as u8;
    self.status.set_c(check_bit(*addr, Bit::Zero));
    *addr >>= 1;
    *addr |= carry << 7;
    self.set_z_n_flags(*addr);
  }

  // Jumps & Calls

  fn jmp(&mut self, addr: u16) {
    self.pc = addr;
  }

  fn jsr(&mut self) {
    // Jump to a subroutine
    self.push_u16(self.pc + 2);
    self.pc = self.read_u16(self.pc + 1);
  }

  fn rts(&mut self) {
    self.pc = self.pull_u16();
  }

  // Branches

  fn bcc(&mut self, val: u8) {
    if !self.status.get_c() {
      self.pc += val as u16;
    }
  }

  fn bcs(&mut self, val: u8) {
    if self.status.get_c() {
      self.pc += val as u16;
    }
  }

  fn beq(&mut self, val: u8) {
    if self.status.get_z() {
      self.pc += val as u16;
    }
  }

  fn bmi(&mut self, val: u8) {
    if self.status.get_n() {
      self.pc += val as u16;
    }
  }

  fn bne(&mut self, val: u8) {
    if !self.status.get_z() {
      self.pc += val as u16;
    }
  }

  fn bpl(&mut self, val: u8) {
    if !self.status.get_n() {
      self.pc += val as u16;
    }
  }

  fn bvc(&mut self, val: u8) {
    if !self.status.get_v() {
      self.pc += val as u16;
    }
  }

  fn bvs(&mut self, val: u8) {
    if self.status.get_v() {
      self.pc += val as u16;
    }
  }

  // Status Flag Changes

  fn clc(&mut self) {
    self.status.set_c(false);
  }

  fn cld(&mut self) {
    self.status.set_d(false);
  }

  fn cli(&mut self) {
    self.status.set_i(false);
  }

  fn clv(&mut self) {
    self.status.set_v(false);
  }

  fn sec(&mut self) {
    self.status.set_c(true);
  }

  fn sed(&mut self) {
    self.status.set_d(true);
  }

  fn sei(&mut self) {
    self.status.set_i(true);
  }

  // System Functions

  fn brk(&mut self) {
    // TODO: ignore this if the i flag is set?

    self.push_u16(self.pc);
    self.push(self.status.bits);
    self.pc = self.read_u16(IRQ_BRK_VEC);
    self.status.set_b(true);
  }

  fn rti(&mut self) {
    self.status.bits = self.pull();
    self.pc = self.pull_u16();
  }

  fn nop(&mut self) {
    // nothing
  }
}
