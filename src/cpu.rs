use crate::bit::*;
use crate::bus::Bus;
use crate::opcodes::{OpCode::*, OpInfo, OPCODES_MAP};
use crate::status::*;

const NMI_VEC: u16 = 0xfffa;
const RESET_VEC: u16 = 0xfffc;
const IRQ_BRK_VEC: u16 = 0xfffe;

const HZ_NTSC: f32 = 1.0 / 1789773.0;
const HZ_PAL: f32 = 1.0 / 1662607.0;

#[derive(Debug, PartialEq)]
pub enum AddressMode {
  Implicit,
  Accumulator, // A
  Immediate,   // #$00
  ZeroPage,    // $00
  ZeroPageX,   // $00,X
  ZeroPageY,   // $00,Y
  Absolute,    // $0000
  AbsoluteX,   // $0000,X
  AbsoluteY,   // $0000,Y
  Indirect,    // ($0000)
  IndirectX,   // ($00,X)
  IndirectY,   // ($00),Y
}

use AddressMode::*;

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
    self.s = 0xff;
    self.a = 0;
    self.x = 0;
    self.y = 0;
    self.status = Status::new();
  }

  fn process(&mut self) {
    loop {
      let opcode = self.read_pc();
      self.inc_pc(1);

      let op = OPCODES_MAP[opcode as usize];

      match op.op {
        BRK => return,

        TAX => self.tax(),
        TXA => self.txa(),
        DEX => self.dex(),
        INX => self.inx(),
        TAY => self.tay(),
        TYA => self.tya(),
        DEY => self.dey(),
        INY => self.iny(),

        ADC => self.adc(&op.mode),
        AND => self.and(&op.mode),
        ASL => self.asl(&op.mode),
        BIT => self.bit(&op.mode),

        BPL => self.bpl(),
        BMI => self.bmi(),
        BVC => self.bvc(),
        BVS => self.bvs(),
        BCC => self.bcc(),
        BCS => self.bcs(),
        BNE => self.bne(),
        BEQ => self.beq(),

        CMP => self.cmp(&op.mode, self.a),
        CPX => self.cmp(&op.mode, self.x),
        CPY => self.cmp(&op.mode, self.y),

        CLC => self.status.set_c(false),
        SEC => self.status.set_c(true),
        CLI => self.status.set_i(false),
        SEI => self.status.set_i(true),
        CLV => self.status.set_v(false),
        CLD => self.status.set_d(false),
        SED => self.status.set_d(true),

        STA => self.sta(&op.mode),
        LDA => self.lda(&op.mode),

        _ => panic!("instruction {:?} not implemented", op),
      }

      self.inc_pc(op.len - 1);
    }
  }

  fn inc_pc(&mut self, inc: u8) {
    self.pc += inc as u16;
  }

  fn read_pc(&self) -> u8 {
    self.read(self.pc)
  }

  fn read_pc_u16(&self) -> u16 {
    self.read_u16(self.pc)
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

  fn get_operand_address(&self, mode: &AddressMode) -> u16 {
    match mode {
      Implicit | Accumulator => panic!("{:?} address mode has no operand", mode),
      Immediate => self.pc,
      ZeroPage => self.read_pc() as u16,
      ZeroPageX => self.read_pc().wrapping_add(self.x) as u16,
      ZeroPageY => self.read_pc().wrapping_add(self.y) as u16,
      Absolute => self.read_pc_u16(),
      AbsoluteX => self.read_pc_u16().wrapping_add(self.x as u16),
      AbsoluteY => self.read_pc_u16().wrapping_add(self.y as u16),
      IndirectX => self
        .bus
        .wrapping_read_u16(self.read_pc().wrapping_add(self.x)),
      IndirectY => self
        .bus
        .wrapping_read_u16(self.read_pc())
        .wrapping_add(self.y as u16),
    }
  }

  // Load / Store Operations

  // first 3 set negative & zero
  fn lda(&mut self, mode: &AddressMode) {
    let addr = self.get_operand_address(&mode);
    let val = self.bus.read(addr);

    self.a = val;
    self.set_z_n_flags(val);
  }

  fn ldx(&mut self, mem: &u8) {
    self.x = *mem;
    self.set_z_n_flags(self.x);
  }

  fn ldy(&mut self, mem: &u8) {
    self.y = *mem;
    self.set_z_n_flags(self.y);
  }

  fn sta(&mut self, mode: &AddressMode) {
    let addr = self.get_operand_address(mode);
    self.write(addr, self.a);
  }

  fn stx(&mut self, mem: &mut u8) {
    *mem = self.x;
  }

  fn sty(&mut self, mem: &mut u8) {
    *mem = self.y;
  }

  // Register Transfers

  fn tax(&mut self) {
    self.x = self.a;
    self.set_z_n_flags(self.x);
  }

  fn txa(&mut self) {
    self.a = self.x;
    self.set_z_n_flags(self.a);
  }

  fn tay(&mut self) {
    self.y = self.a;
    self.set_z_n_flags(self.y);
  }

  fn tya(&mut self) {
    self.a = self.y;
    self.set_z_n_flags(self.a);
  }

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

  fn and(&mut self, mode: &AddressMode) {
    let addr = self.get_operand_address(mode);
    let val = self.read(addr);
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

  fn bit(&mut self, mode: &AddressMode) {
    let addr = self.get_operand_address(mode);
    let val = self.read(addr);

    self.status.set_z(self.a & val == 0);
    self.status.set_v(check_bit(val, Bit::Six));
    self.status.set_n(check_bit(val, Bit::Seven));
  }

  // Arithmetic

  fn adc(&mut self, mode: &AddressMode) {
    let addr = self.get_operand_address(mode);
    let val = self.read(addr);

    let initial_a = self.a;
    self.a = self
      .a
      .wrapping_add(val)
      .wrapping_add(self.status.get_c() as u8);
    self.status.set_c(initial_a > self.a);
    self
      .status
      .set_v(self.status.get_c() ^ (check_bit(initial_a, Bit::Seven)));
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

  fn cmp(&mut self, mode: &AddressMode, with: u8) {
    let addr = self.get_operand_address(mode);
    let val = self.read(addr);
    self.status.set_c(with >= val);
    self.set_z_n_flags(with.wrapping_sub(val));
  }

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
    self.y = self.y.wrapping_add(1);
    self.set_z_n_flags(self.y)
  }

  fn dec(&mut self, mem: &mut u8) {
    *mem -= 1;
    self.set_z_n_flags(*mem);
  }

  fn dex(&mut self) {
    self.x = self.x.wrapping_sub(1);
    self.set_z_n_flags(self.x)
  }

  fn dey(&mut self) {
    self.y = self.y.wrapping_sub(1);
    self.set_z_n_flags(self.y)
  }

  // Shifts

  fn asl(&mut self, mode: &AddressMode) {
    let mut addr: u16 = 0;
    let mut val: u8;

    if *mode == Accumulator {
      val = self.a
    } else {
      addr = self.get_operand_address(mode);
      val = self.read(addr);
    }

    self.status.set_c(check_bit(val, Bit::Seven));
    val <<= 1;
    self.set_z_n_flags(val);

    if *mode == Accumulator {
      self.a = val;
    } else {
      self.write(addr, val);
    }
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

  fn bcc(&mut self) {
    if !self.status.get_c() {
      self.pc += self.read_pc() as u16;
    }
  }

  fn bcs(&mut self) {
    if self.status.get_c() {
      self.pc += self.read_pc() as u16;
    }
  }

  fn beq(&mut self) {
    if self.status.get_z() {
      self.pc += self.read_pc() as u16;
    }
  }

  fn bmi(&mut self) {
    if self.status.get_n() {
      self.pc += self.read_pc() as u16;
    }
  }

  fn bne(&mut self) {
    if !self.status.get_z() {
      self.pc += self.read_pc() as u16;
    }
  }

  fn bpl(&mut self) {
    if !self.status.get_n() {
      self.pc += self.read_pc() as u16;
    }
  }

  fn bvc(&mut self) {
    if !self.status.get_v() {
      self.pc += self.read_pc() as u16;
    }
  }

  fn bvs(&mut self) {
    if self.status.get_v() {
      self.pc += self.read_pc() as u16;
    }
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

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_0xa9_lda_immediate_load_data() {
    let mut bus = vec![0xa9, 0x05, 0x00];
    let mut cpu = Cpu::new(&mut bus);
    cpu.process();
    assert_eq!(cpu.a, 0x05);
    assert!(cpu.status.get_z() == false);
    assert!(cpu.status.get_n() == false);
  }

  #[test]
  fn test_0xa9_lda_zero_flag() {
    let mut bus = vec![0xa9, 0x00, 0x00];
    let mut cpu = Cpu::new(&mut bus);
    cpu.process();
    assert!(cpu.status.get_z() == true);
  }

  #[test]
  fn test_0xaa_tax_move_a_to_x() {
    let mut bus = vec![0xaa, 0x00];
    let mut cpu = Cpu::new(&mut bus);
    cpu.a = 10;
    cpu.process();
    assert_eq!(cpu.x, 10)
  }

  #[test]
  fn test_5_ops_working_together() {
    let mut bus = vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00];
    let mut cpu = Cpu::new(&mut bus);
    cpu.process();
    assert_eq!(cpu.x, 0xc1)
  }

  #[test]
  fn test_inx_overflow() {
    let mut bus = vec![0xe8, 0xe8, 0x00];
    let mut cpu = Cpu::new(&mut bus);
    cpu.x = 0xff;
    cpu.process();
    assert_eq!(cpu.x, 1)
  }

  #[test]
  fn test_0xa5_lda_zero_page() {
    let mut bus = vec![0; 0x10];
    bus[0] = 0xa5;
    bus[1] = 0x09;
    bus[0x09] = 0xba;

    let mut cpu = Cpu::new(&mut bus);
    cpu.process();
    assert_eq!(cpu.a, 0xba)
  }

  #[test]
  fn test_0xb5_lda_zero_page_x() {
    let mut bus = vec![0; 0x10];
    bus[0] = 0xb5;
    bus[1] = 0x05;
    bus[0x07] = 0xfa;

    let mut cpu = Cpu::new(&mut bus);
    cpu.x = 0x02;

    cpu.process();
    assert_eq!(cpu.a, 0xfa)
  }

  #[test]
  fn test_0xb1_lda_indirect_y() {
    let mut bus = vec![0; 0x1_0000];
    bus[0] = 0xb1;
    bus[1] = 0x03;
    bus[3] = 0x03;
    bus[4] = 0x04;
    bus[0x0406] = 0xba;

    let mut cpu = Cpu::new(&mut bus);
    cpu.y = 0x03;

    cpu.process();
    assert_eq!(cpu.a, 0xba)
  }

  #[test]
  fn test_0x9d_sta_absolute_x() {
    let mut bus = vec![0; 0x1_0000];
    bus[0] = 0x9d;
    bus[1] = 0x02;
    bus[2] = 0x03;

    let mut cpu = Cpu::new(&mut bus);
    cpu.x = 0x04;
    cpu.a = 0xaa;

    cpu.process();
    assert_eq!(bus[0x0306], 0xaa)
  }

  #[test]
  fn test_0x69_adc_immediate() {
    let mut bus = vec![0x69, 0x20, 0x00];
    let mut cpu = Cpu::new(&mut bus);
    cpu.a = 0x05;
    cpu.status.set_c(true);

    cpu.process();
    assert_eq!(cpu.a, 0x26);
    assert_eq!(cpu.status.get_c(), false);
    assert_eq!(cpu.status.get_z(), false)
  }

  #[test]
  fn test_0x69_adc_immediate_negative() {
    let mut bus = vec![0x69, 0x7f, 0x00];
    let mut cpu = Cpu::new(&mut bus);
    cpu.a = 0x08;

    cpu.process();
    assert_eq!(cpu.a, 0x87);
    assert_eq!(cpu.status.get_c(), false);
    assert_eq!(cpu.status.get_n(), true);
    assert_eq!(cpu.status.get_z(), false)
  }

  #[test]
  fn test_0x75_adc_zero_page_x_zero() {
    let mut bus = vec![0x75, 0x02, 0x00, 0x07];
    let mut cpu = Cpu::new(&mut bus);
    cpu.x = 0x01;
    cpu.a = 0xf9;

    cpu.process();
    assert_eq!(cpu.a, 0x00);
    assert_eq!(cpu.status.get_c(), true);
    assert_eq!(cpu.status.get_n(), false);
    assert_eq!(cpu.status.get_z(), true)
  }

  #[test]
  fn test_0x39_and_absolute_y() {
    let mut bus = vec![0; 0x1_0000];
    bus[0] = 0x39;
    bus[1] = 0x01;
    bus[2] = 0x20;
    bus[0x2003] = 0x0f;
    let mut cpu = Cpu::new(&mut bus);
    cpu.y = 0x02;
    cpu.a = 0xf9;

    cpu.process();
    assert_eq!(cpu.a, 0x09);
    assert_eq!(cpu.status.get_n(), false);
    assert_eq!(cpu.status.get_z(), false)
  }

  #[test]
  fn test_0x0a_asl_accumulator() {
    let mut bus = vec![0x0a, 0x00];
    let mut cpu = Cpu::new(&mut bus);
    cpu.a = 0x80;

    cpu.process();
    assert_eq!(cpu.a, 0x00);
    assert_eq!(cpu.status.get_n(), false);
    assert_eq!(cpu.status.get_z(), true);
    assert_eq!(cpu.status.get_c(), true);
  }

  #[test]
  fn test_0x24_bit_zero_page() {
    let mut bus = vec![0x24, 0x03, 0x00, 0xf0];
    let mut cpu = Cpu::new(&mut bus);
    cpu.a = 0xf0;

    cpu.process();
    assert_eq!(cpu.status.get_n(), true);
    assert_eq!(cpu.status.get_v(), true);
    assert_eq!(cpu.status.get_z(), false);
  }

  #[test]
  fn test_set_carry_branch_carry() {
    // set carry, branch on carry, load 0xfa into a
    let mut bus = vec![0x38, 0xb0, 0x01, 0x00, 0xa9, 0xfa, 0x00];
    let mut cpu = Cpu::new(&mut bus);

    cpu.process();
    assert_eq!(cpu.a, 0xfa);
  }

  #[test]
  fn test_0xc9_cmp_immediate() {
    let mut bus = vec![0xc9, 0x0a, 0x00];
    let mut cpu = Cpu::new(&mut bus);
    cpu.a = 0x09;

    cpu.process();
    assert_eq!(cpu.a, 0x09); // didn't modify
    assert_eq!(cpu.status.get_z(), false);
    assert_eq!(cpu.status.get_c(), false);
    assert_eq!(cpu.status.get_n(), true);
  }

  #[test]
  fn test_0xe0_cpx_immediate() {
    let mut bus = vec![0xe0, 0x0a, 0x00];
    let mut cpu = Cpu::new(&mut bus);
    cpu.x = 0x0a;

    cpu.process();
    assert_eq!(cpu.x, 0x0a); // didn't modify
    assert_eq!(cpu.status.get_z(), true);
    assert_eq!(cpu.status.get_c(), true);
    assert_eq!(cpu.status.get_n(), false);
  }

  #[test]
  fn test_0xc0_cpy_immediate() {
    let mut bus = vec![0xc0, 0x0a, 0x00];
    let mut cpu = Cpu::new(&mut bus);
    cpu.y = 0xfa;

    cpu.process();
    assert_eq!(cpu.y, 0xfa); // didn't modify
    assert_eq!(cpu.status.get_z(), false);
    assert_eq!(cpu.status.get_c(), true);
    assert_eq!(cpu.status.get_n(), true);
  }
}
