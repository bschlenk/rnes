use crate::bit::*;
use crate::bus::Bus;
use crate::opcodes::{OpCode::*, OpInfo, OPCODES_MAP};
use crate::status::Status;

const STACK_PAGE: u16 = 0x0100;
const NMI_VEC: u16 = 0xfffa;
const RESET_VEC: u16 = 0xfffc;
const IRQ_BRK_VEC: u16 = 0xfffe;

// https://superuser.com/a/606770
const STACK_RESET: u8 = 0xfd;

// const HZ_NTSC: f32 = 1.0 / 1789773.0;
// const HZ_PAL: f32 = 1.0 / 1662607.0;

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
  pub sp: u8,

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
  pub p: Status,

  /** Number of cycles passed since the emulator started. */
  pub cycles: usize,
}

impl<'a> Cpu<'a> {
  pub fn new(bus: &'a mut dyn Bus) -> Cpu<'a> {
    Cpu {
      bus,
      pc: 0,
      sp: 0xff,
      a: 0,
      x: 0,
      y: 0,
      p: Status::default(),
      cycles: 7, // TODO: reset should do this
    }
  }

  pub fn reset(&mut self) {
    self.pc = self.read_u16(RESET_VEC);
    self.sp = STACK_RESET;
    self.a = 0;
    self.x = 0;
    self.y = 0;
    self.p.reset();
    println!("reset pc to {:02x}", self.pc);
  }

  pub fn run(&mut self) {
    self.run_with_callback(|_| {});
  }

  pub fn run_with_callback<F>(&mut self, mut callback: F)
  where
    F: FnMut(&mut Cpu),
  {
    loop {
      if let Some(_nmi) = self.bus.poll_nmi_status() {
        self.interrupt_nmi();
      }

      callback(self);

      let opcode = self.read_pc();
      self.inc_pc(1);

      let op = OPCODES_MAP[opcode as usize];

      /*
      println!(
        "{:04X}  {:02X} {} {:<31}  A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} PPU:{} CYC:{}",
        self.pc - 1,
        opcode,
        match op.len {
          // TODO: remove this when BAD_OPCODE is gone
          0 => "XX XX".to_string(),
          1 => "     ".to_string(),
          2 => format!("{:02X}   ", self.read_pc()),
          3 => format!("{:02X} {:02X}", self.read_pc(), self.read(self.pc + 1)),
          _ => unreachable!(),
        },
        op.to_assembly(self),
        self.a,
        self.x,
        self.y,
        self.p,
        self.sp,
        self.bus.ppu_debug_str(),
        self.cycles,
      );
      */

      let prev_pc = self.pc;
      let prev_cycles = self.cycles;

      match op.op {
        BRK => {
          self.brk();
          return;
        }
        NOP => {
          // unofficial nop may cause extra cycles
          self.boundary_tick(&op);
        }
        RTI => self.rti(),

        TAX => self.tax(),
        TXA => self.txa(),
        DEX => self.dex(),
        INX => self.inx(),
        TAY => self.tay(),
        TYA => self.tya(),
        DEY => self.dey(),
        INY => self.iny(),

        TXS => self.txs(),
        TSX => self.tsx(),
        PHA => self.pha(),
        PLA => self.pla(),
        PHP => self.php(),
        PLP => self.plp(),

        ADC => self.adc(&op, true),
        SBC => self.sbc(&op),
        AND => self.and(&op, true),
        ORA => self.ora(&op, true),
        BIT => self.bit(&op.mode),

        ASL => self.asl(&op.mode),
        LSR => self.lsr(&op.mode),
        ROL => self.rol(&op.mode),
        ROR => self.ror(&op.mode),

        BPL => self.bpl(),
        BMI => self.bmi(),
        BVC => self.bvc(),
        BVS => self.bvs(),
        BCC => self.bcc(),
        BCS => self.bcs(),
        BNE => self.bne(),
        BEQ => self.beq(),

        JMP => self.jmp(&op.mode),

        JSR => self.jsr(),
        RTS => self.rts(),

        CMP => self.cmp(&op, self.a, true),
        CPX => self.cmp(&op, self.x, false),
        CPY => self.cmp(&op, self.y, false),

        DEC => self.dec(&op.mode),
        INC => {
          self.inc(&op.mode);
        }
        EOR => self.eor(&op, true),

        CLC => self.p.remove(Status::CARRY),
        SEC => self.p.insert(Status::CARRY),
        CLI => self.p.remove(Status::INTERRUPT),
        SEI => self.p.insert(Status::INTERRUPT),
        CLV => self.p.remove(Status::OVERFLOW),
        CLD => self.p.remove(Status::DECIMAL),
        SED => self.p.insert(Status::DECIMAL),

        LDA => self.lda(&op),
        LDX => self.ldx(&op),
        LDY => self.ldy(&op),
        STA => self.sta(&op.mode),
        STX => self.stx(&op.mode),
        STY => self.sty(&op.mode),

        // Unofficial Opcodes
        DCP => self.dcp(&op),
        ISB => self.isb(&op.mode),
        LAX => self.lax(&op),
        RLA => self.rla(&op),
        RRA => self.rra(&op),
        SAX => self.sax(&op.mode),
        SLO => self.slo(&op),
        SRE => self.sre(&op),

        _ => panic!("instruction {:?} not implemented", op),
      }

      // only increment pc if the op didn't do it for us, like a JMP
      if prev_pc == self.pc {
        self.inc_pc(op.len - 1);
      }

      // only tick if the op didn't do it for us
      if prev_cycles == self.cycles {
        self.tick(op.cycles as usize);
      }
    }
  }

  fn tick(&mut self, cycles: usize) {
    self.cycles += cycles;
    self.bus.tick(cycles);
  }

  fn interrupt_nmi(&mut self) {
    self.push_u16(self.pc);
    self.push_p(true);
    self.p.insert(Status::INTERRUPT);
    self.tick(2);
    self.pc = self.read_u16(NMI_VEC);
  }

  fn inc_pc(&mut self, inc: u8) {
    self.pc += inc as u16;
  }

  pub fn read_pc(&mut self) -> u8 {
    self.read(self.pc)
  }

  pub fn read_pc_u16(&mut self) -> u16 {
    self.read_u16(self.pc)
  }

  pub fn read(&mut self, addr: u16) -> u8 {
    self.bus.read(addr)
  }

  pub fn read_u16(&mut self, addr: u16) -> u16 {
    self.bus.read_u16(addr)
  }

  pub fn write(&mut self, addr: u16, val: u8) {
    self.bus.write(addr, val);
  }

  pub fn write_u16(&mut self, addr: u16, val: u16) {
    self.bus.write_u16(addr, val)
  }

  fn push(&mut self, val: u8) {
    self.write(STACK_PAGE | (self.sp as u16), val);
    self.sp = self.sp.wrapping_sub(1);
  }

  fn pull(&mut self) -> u8 {
    self.sp = self.sp.wrapping_add(1);
    self.read(STACK_PAGE | (self.sp as u16))
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

  /// Push the status reg to the stack. Bit 4 is set when the push was triggered
  /// by an instruction. Bit 5 is always set.
  /// http://wiki.nesdev.com/w/index.php/Status_flags#The_B_flag
  fn push_p(&mut self, interrupt: bool) {
    let mut p = self.p.clone();
    p.set(Status::BREAK, !interrupt);
    p.insert(Status::BREAK2);
    self.push(p.bits());
  }

  fn set_z_n_flags(&mut self, val: u8) {
    self.p.set(Status::ZERO, val == 0);
    self.p.set(Status::NEGATIVE, check_bit(val, Bit::Seven));
  }

  pub fn get_operand_address(&mut self, mode: &AddressMode) -> u16 {
    match mode {
      Implicit | Accumulator => panic!("{:?} address mode has no operand", mode),
      Immediate => self.pc,
      ZeroPage => self.read_pc() as u16,
      ZeroPageX => self.read_pc().wrapping_add(self.x) as u16,
      ZeroPageY => self.read_pc().wrapping_add(self.y) as u16,
      Absolute => self.read_pc_u16(),
      AbsoluteX => self.read_pc_u16().wrapping_add(self.x as u16),
      AbsoluteY => self.read_pc_u16().wrapping_add(self.y as u16),
      Indirect => {
        // Indirect is only used by JMP, and contains a bug - there is no carry,
        // so reading from the last byte of a page rolls over to the first byte
        // of that page.
        let lo_addr = self.read_pc_u16();
        let (lo_tmp, hi_tmp) = split_u16(lo_addr);
        let hi_addr = make_u16(lo_tmp.wrapping_add(1), hi_tmp);

        make_u16(self.read(lo_addr), self.read(hi_addr))
      }
      IndirectX => {
        let addr = self.read_pc().wrapping_add(self.x);
        self.bus.wrapping_read_u16(addr)
      }
      IndirectY => {
        let addr = self.read_pc();
        self.bus.wrapping_read_u16(addr).wrapping_add(self.y as u16)
      }
    }
  }

  fn boundary_crossed(&mut self, mode: &AddressMode) -> bool {
    match mode {
      AbsoluteX => {
        let old = self.read_pc_u16();
        let new = old.wrapping_add(self.x as u16);
        page_crossed(old, new)
      }
      AbsoluteY => {
        let old = self.read_pc_u16();
        let new = old.wrapping_add(self.y as u16);
        page_crossed(old, new)
      }
      IndirectY => {
        let addr = self.read_pc();
        let old = self.bus.wrapping_read_u16(addr);
        let new = old.wrapping_add(self.y as u16);
        page_crossed(old, new)
      }
      _ => false,
    }
  }

  fn boundary_tick(&mut self, op: &OpInfo) {
    if self.boundary_crossed(&op.mode) {
      self.tick((op.cycles + 1) as usize);
    }
  }

  // Load / Store Operations

  fn lda(&mut self, op: &OpInfo) {
    let addr = self.get_operand_address(&op.mode);
    let val = self.bus.read(addr);

    self.a = val;
    self.set_z_n_flags(val);

    self.boundary_tick(op);
  }

  fn ldx(&mut self, op: &OpInfo) {
    let addr = self.get_operand_address(&op.mode);
    let val = self.bus.read(addr);

    self.x = val;
    self.set_z_n_flags(val);

    self.boundary_tick(op);
  }

  fn ldy(&mut self, op: &OpInfo) {
    let addr = self.get_operand_address(&op.mode);
    let val = self.bus.read(addr);

    self.y = val;
    self.set_z_n_flags(val);

    self.boundary_tick(op);
  }

  fn sta(&mut self, mode: &AddressMode) {
    let addr = self.get_operand_address(mode);
    self.write(addr, self.a);
  }

  fn stx(&mut self, mode: &AddressMode) {
    let addr = self.get_operand_address(mode);
    self.write(addr, self.x);
  }

  fn sty(&mut self, mode: &AddressMode) {
    let addr = self.get_operand_address(mode);
    self.write(addr, self.y);
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

  fn txs(&mut self) {
    self.sp = self.x;
  }

  fn tsx(&mut self) {
    self.x = self.sp;
    self.set_z_n_flags(self.x);
  }

  fn pha(&mut self) {
    self.push(self.a);
  }

  fn php(&mut self) {
    self.push_p(false);
  }

  fn pla(&mut self) {
    self.a = self.pull();
    self.set_z_n_flags(self.a);
  }

  fn plp(&mut self) {
    let bits = self.pull();
    self.p.update(bits);
    // TODO: find the docs for these, I don't get it
    self.p.remove(Status::BREAK);
    self.p.insert(Status::BREAK2);
  }

  // Logical

  fn and(&mut self, op: &OpInfo, check_bounds: bool) {
    let addr = self.get_operand_address(&op.mode);
    let val = self.read(addr);
    self.a &= val;
    self.set_z_n_flags(self.a);

    // TODO: this is getting ugly...
    if check_bounds {
      self.boundary_tick(op);
    }
  }

  fn eor(&mut self, op: &OpInfo, check_bounds: bool) {
    let addr = self.get_operand_address(&op.mode);
    let val = self.read(addr);
    self.a ^= val;
    self.set_z_n_flags(self.a);

    if check_bounds {
      self.boundary_tick(op);
    }
  }

  fn ora(&mut self, op: &OpInfo, check_boundary: bool) {
    let addr = self.get_operand_address(&op.mode);
    let val = self.read(addr);
    self.a |= val;
    self.set_z_n_flags(self.a);

    // TODO: this is getting ugly...
    if check_boundary {
      self.boundary_tick(op);
    }
  }

  fn bit(&mut self, mode: &AddressMode) {
    let addr = self.get_operand_address(mode);
    let val = self.read(addr);

    self.p.set(Status::ZERO, self.a & val == 0);
    self.p.set(Status::OVERFLOW, check_bit(val, Bit::Six));
    self.p.set(Status::NEGATIVE, check_bit(val, Bit::Seven));
  }

  // Arithmetic

  fn adc(&mut self, op: &OpInfo, check_bounds: bool) {
    let addr = self.get_operand_address(&op.mode);
    let val = self.read(addr);
    self.add_to_a(val);

    // TODO: ugly
    if check_bounds {
      self.boundary_tick(op);
    }
  }

  fn sbc(&mut self, op: &OpInfo) {
    let addr = self.get_operand_address(&op.mode);
    let val = self.read(addr);
    self.sub_from_a(val);

    self.boundary_tick(op);
  }

  fn add_to_a(&mut self, val: u8) {
    let initial_a = self.a;
    let res = (self.a as u16)
      .wrapping_add(val as u16)
      .wrapping_add(self.p.contains(Status::CARRY) as u16);
    self.a = res as u8;
    self.p.set(Status::CARRY, res > 0xff);
    self.p.set(
      Status::OVERFLOW,
      check_bit((initial_a ^ self.a) & (val ^ self.a), Bit::Seven),
    );
    self.set_z_n_flags(self.a);
  }

  fn sub_from_a(&mut self, val: u8) {
    // sub 1 due to carry
    self.add_to_a(val.wrapping_neg().wrapping_sub(1));
  }

  fn cmp(&mut self, op: &OpInfo, with: u8, check_bounds: bool) {
    let addr = self.get_operand_address(&op.mode);
    let val = self.read(addr);
    self.p.set(Status::CARRY, with >= val);
    self.set_z_n_flags(with.wrapping_sub(val));

    if check_bounds {
      self.boundary_tick(op);
    }
  }

  // Increments & Decrements

  fn inc(&mut self, mode: &AddressMode) -> u8 {
    let addr = self.get_operand_address(mode);
    let val = self.read(addr).wrapping_add(1);
    self.write(addr, val);
    self.set_z_n_flags(val);
    // return is used in unofficial opcodes
    val
  }

  fn inx(&mut self) {
    self.x = self.x.wrapping_add(1);
    self.set_z_n_flags(self.x)
  }

  fn iny(&mut self) {
    self.y = self.y.wrapping_add(1);
    self.set_z_n_flags(self.y)
  }

  fn dec(&mut self, mode: &AddressMode) {
    let addr = self.get_operand_address(mode);
    let val = self.read(addr).wrapping_sub(1);
    self.write(addr, val);
    self.set_z_n_flags(val);
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

    self.p.set(Status::CARRY, check_bit(val, Bit::Seven));
    val <<= 1;
    self.set_z_n_flags(val);

    if *mode == Accumulator {
      self.a = val;
    } else {
      self.write(addr, val);
    }
  }

  fn lsr(&mut self, mode: &AddressMode) {
    let mut addr: u16 = 0;
    let mut val: u8;

    if *mode == Accumulator {
      val = self.a
    } else {
      addr = self.get_operand_address(mode);
      val = self.read(addr);
    }

    self.p.set(Status::CARRY, check_bit(val, Bit::Zero));
    val >>= 1;
    self.set_z_n_flags(val);

    if *mode == Accumulator {
      self.a = val;
    } else {
      self.write(addr, val);
    }
  }

  fn rol(&mut self, mode: &AddressMode) {
    let mut addr: u16 = 0;
    let mut val: u8;

    if *mode == Accumulator {
      val = self.a
    } else {
      addr = self.get_operand_address(mode);
      val = self.read(addr);
    }

    let carry = self.p.contains(Status::CARRY) as u8;
    self.p.set(Status::CARRY, check_bit(val, Bit::Seven));
    val <<= 1;
    val |= carry;
    self.set_z_n_flags(val);

    if *mode == Accumulator {
      self.a = val;
    } else {
      self.write(addr, val);
    }
  }

  fn ror(&mut self, mode: &AddressMode) {
    let mut addr: u16 = 0;
    let mut val: u8;

    if *mode == Accumulator {
      val = self.a
    } else {
      addr = self.get_operand_address(mode);
      val = self.read(addr);
    }

    let carry = self.p.contains(Status::CARRY) as u8;
    self.p.set(Status::CARRY, check_bit(val, Bit::Zero));
    val >>= 1;
    val |= carry << 7;
    self.set_z_n_flags(val);

    if *mode == Accumulator {
      self.a = val;
    } else {
      self.write(addr, val);
    }
  }

  // Jumps & Calls

  fn jmp(&mut self, mode: &AddressMode) {
    let addr = self.get_operand_address(mode);
    self.pc = addr;
  }

  fn jsr(&mut self) {
    self.push_u16(self.pc + 1);
    self.pc = self.read_u16(self.pc);
  }

  fn rts(&mut self) {
    self.pc = self.pull_u16() + 1;
  }

  // Branches

  fn bcc(&mut self) {
    self.branch(Status::CARRY, false);
  }

  fn bcs(&mut self) {
    self.branch(Status::CARRY, true);
  }

  fn bne(&mut self) {
    self.branch(Status::ZERO, false);
  }

  fn beq(&mut self) {
    self.branch(Status::ZERO, true);
  }

  fn bpl(&mut self) {
    self.branch(Status::NEGATIVE, false);
  }

  fn bmi(&mut self) {
    self.branch(Status::NEGATIVE, true);
  }

  fn bvc(&mut self) {
    self.branch(Status::OVERFLOW, false);
  }

  fn bvs(&mut self) {
    self.branch(Status::OVERFLOW, true);
  }

  fn branch(&mut self, status: Status, expect: bool) {
    if self.p.contains(status) == expect {
      let b = self.read_pc();
      // add 1 to skip over op
      self.inc_pc(1);

      let old_pc = self.pc;
      self.pc = add_signed(old_pc, b);

      // branch taken, we want to increment cycle by 3, or 4 if page boundary crossed
      self.tick(if page_crossed(old_pc, self.pc) { 4 } else { 3 });
    }
  }

  // System Functions

  fn brk(&mut self) {
    // TODO: ignore this if the i flag is set?

    self.push_u16(self.pc);
    self.push(self.p.bits());
    self.pc = self.read_u16(IRQ_BRK_VEC);
    self.p.insert(Status::BREAK);
  }

  fn rti(&mut self) {
    let bits = self.pull();
    self.p.update(bits);
    self.pc = self.pull_u16();
    self.p.remove(Status::BREAK);
    self.p.insert(Status::BREAK2);
  }

  // Unofficial Opcodes

  fn dcp(&mut self, op: &OpInfo) {
    let addr = self.get_operand_address(&op.mode);
    let val = self.read(addr).wrapping_sub(1);
    self.write(addr, val);
    self.cmp(op, self.a, false);
  }

  fn isb(&mut self, mode: &AddressMode) {
    let val = self.inc(mode);
    self.sub_from_a(val);
  }

  fn lax(&mut self, op: &OpInfo) {
    let addr = self.get_operand_address(&op.mode);
    let val = self.read(addr);
    self.a = val;
    self.x = val;
    self.set_z_n_flags(val);

    self.boundary_tick(op);
  }

  fn rla(&mut self, op: &OpInfo) {
    self.rol(&op.mode);
    self.and(op, false);
  }

  fn rra(&mut self, op: &OpInfo) {
    self.ror(&op.mode);
    self.adc(op, false);
  }

  fn sax(&mut self, mode: &AddressMode) {
    let addr = self.get_operand_address(mode);
    self.write(addr, self.a & self.x);
  }

  fn slo(&mut self, op: &OpInfo) {
    self.asl(&op.mode);
    self.ora(op, false);
  }

  fn sre(&mut self, op: &OpInfo) {
    self.lsr(&op.mode);
    self.eor(op, false);
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn test_0xa9_lda_immediate_load_data() {
    let mut bus = vec![0xa9, 0x05, 0x00];
    let mut cpu = Cpu::new(&mut bus);
    cpu.run();
    assert_eq!(cpu.a, 0x05);
    assert!(!cpu.p.contains(Status::ZERO));
    assert!(!cpu.p.contains(Status::NEGATIVE));
  }

  #[test]
  fn test_0xa9_lda_zero_flag() {
    let mut bus = vec![0xa9, 0x00, 0x00];
    let mut cpu = Cpu::new(&mut bus);
    cpu.run();
    assert!(cpu.p.contains(Status::ZERO));
  }

  #[test]
  fn test_0xaa_tax_move_a_to_x() {
    let mut bus = vec![0xaa, 0x00];
    let mut cpu = Cpu::new(&mut bus);
    cpu.a = 10;
    cpu.run();
    assert_eq!(cpu.x, 10)
  }

  #[test]
  fn test_5_ops_working_together() {
    let mut bus = vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00];
    let mut cpu = Cpu::new(&mut bus);
    cpu.run();
    assert_eq!(cpu.x, 0xc1)
  }

  #[test]
  fn test_inx_overflow() {
    let mut bus = vec![0xe8, 0xe8, 0x00];
    let mut cpu = Cpu::new(&mut bus);
    cpu.x = 0xff;
    cpu.run();
    assert_eq!(cpu.x, 1)
  }

  #[test]
  fn test_0xa5_lda_zero_page() {
    let mut bus = vec![0; 0x10];
    bus[0] = 0xa5;
    bus[1] = 0x09;
    bus[0x09] = 0xba;

    let mut cpu = Cpu::new(&mut bus);
    cpu.run();
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

    cpu.run();
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

    cpu.run();
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

    cpu.run();
    assert_eq!(bus[0x0306], 0xaa)
  }

  #[test]
  fn test_0x69_adc_immediate() {
    let mut bus = vec![0x69, 0x20, 0x00];
    let mut cpu = Cpu::new(&mut bus);
    cpu.a = 0x05;
    cpu.p.insert(Status::CARRY);

    cpu.run();
    assert_eq!(cpu.a, 0x26);
    assert_eq!(cpu.p.contains(Status::CARRY), false);
    assert_eq!(cpu.p.contains(Status::ZERO), false)
  }

  #[test]
  fn test_0x69_adc_immediate_overflow() {
    // load 0x7f into A, add 0x7f to A
    let mut bus = vec![0xa9, 0x7f, 0x69, 0x7f, 0x00];
    let mut cpu = Cpu::new(&mut bus);

    cpu.run();
    assert_eq!(cpu.a, 0xfe);
    assert_eq!(cpu.p.contains(Status::OVERFLOW), true);
    assert_eq!(cpu.p.contains(Status::NEGATIVE), true)
  }

  #[test]
  fn test_0x69_adc_immediate_negative() {
    let mut bus = vec![0x69, 0x7f, 0x00];
    let mut cpu = Cpu::new(&mut bus);
    cpu.a = 0x08;

    cpu.run();
    assert_eq!(cpu.a, 0x87);
    assert_eq!(cpu.p.contains(Status::CARRY), false);
    assert_eq!(cpu.p.contains(Status::NEGATIVE), true);
    assert_eq!(cpu.p.contains(Status::ZERO), false)
  }

  #[test]
  fn test_0x75_adc_zero_page_x_zero() {
    let mut bus = vec![0x75, 0x02, 0x00, 0x07];
    let mut cpu = Cpu::new(&mut bus);
    cpu.x = 0x01;
    cpu.a = 0xf9;

    cpu.run();
    assert_eq!(cpu.a, 0x00);
    assert_eq!(cpu.p.contains(Status::CARRY), true);
    assert_eq!(cpu.p.contains(Status::NEGATIVE), false);
    assert_eq!(cpu.p.contains(Status::ZERO), true)
  }

  #[test]
  fn test_0xe9_sbc_zero_from_ff() {
    // load 0xff into A, subtract 0
    let mut bus = vec![0xa9, 0xff, 0xe9, 0x00, 0x00];
    let mut cpu = Cpu::new(&mut bus);
    cpu.p.insert(Status::CARRY);

    cpu.run();
    assert_eq!(cpu.a, 0xff);
    assert_eq!(cpu.p.contains(Status::CARRY), true);
    assert_eq!(cpu.p.contains(Status::NEGATIVE), true);
    assert_eq!(cpu.p.contains(Status::ZERO), false)
  }

  #[test]
  fn test_0xe9_sbc_40_from_40() {
    // load 0x40 into A, subtract 0x40
    let mut bus = vec![0xa9, 0x40, 0xe9, 0x40, 0x00];
    let mut cpu = Cpu::new(&mut bus);
    cpu.p.insert(Status::CARRY);

    cpu.run();
    assert_eq!(cpu.a, 0x00);
    assert_eq!(cpu.p.contains(Status::OVERFLOW), false);
    assert_eq!(cpu.p.contains(Status::CARRY), true);
    assert_eq!(cpu.p.contains(Status::NEGATIVE), false);
    assert_eq!(cpu.p.contains(Status::ZERO), true);
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

    cpu.run();
    assert_eq!(cpu.a, 0x09);
    assert_eq!(cpu.p.contains(Status::NEGATIVE), false);
    assert_eq!(cpu.p.contains(Status::ZERO), false)
  }

  #[test]
  fn test_0x0a_asl_accumulator() {
    let mut bus = vec![0x0a, 0x00];
    let mut cpu = Cpu::new(&mut bus);
    cpu.a = 0x80;

    cpu.run();
    assert_eq!(cpu.a, 0x00);
    assert_eq!(cpu.p.contains(Status::NEGATIVE), false);
    assert_eq!(cpu.p.contains(Status::ZERO), true);
    assert_eq!(cpu.p.contains(Status::CARRY), true);
  }

  #[test]
  fn test_0x24_bit_zero_page() {
    let mut bus = vec![0x24, 0x03, 0x00, 0xf0];
    let mut cpu = Cpu::new(&mut bus);
    cpu.a = 0xf0;

    cpu.run();
    assert_eq!(cpu.p.contains(Status::NEGATIVE), true);
    assert_eq!(cpu.p.contains(Status::OVERFLOW), true);
    assert_eq!(cpu.p.contains(Status::ZERO), false);
  }

  #[test]
  fn test_set_carry_branch_carry() {
    // set carry, branch on carry, load 0xfa into a
    let mut bus = vec![0x38, 0xb0, 0x01, 0x00, 0xa9, 0xfa, 0x00];
    let mut cpu = Cpu::new(&mut bus);

    cpu.run();
    assert_eq!(cpu.a, 0xfa);
  }

  #[test]
  fn test_0xc9_cmp_immediate() {
    let mut bus = vec![0xc9, 0x0a, 0x00];
    let mut cpu = Cpu::new(&mut bus);
    cpu.a = 0x09;

    cpu.run();
    assert_eq!(cpu.a, 0x09); // didn't modify
    assert_eq!(cpu.p.contains(Status::ZERO), false);
    assert_eq!(cpu.p.contains(Status::CARRY), false);
    assert_eq!(cpu.p.contains(Status::NEGATIVE), true);
  }

  #[test]
  fn test_0xe0_cpx_immediate() {
    let mut bus = vec![0xe0, 0x0a, 0x00];
    let mut cpu = Cpu::new(&mut bus);
    cpu.x = 0x0a;

    cpu.run();
    assert_eq!(cpu.x, 0x0a); // didn't modify
    assert_eq!(cpu.p.contains(Status::ZERO), true);
    assert_eq!(cpu.p.contains(Status::CARRY), true);
    assert_eq!(cpu.p.contains(Status::NEGATIVE), false);
  }

  #[test]
  fn test_0xc0_cpy_immediate() {
    let mut bus = vec![0xc0, 0x0a, 0x00];
    let mut cpu = Cpu::new(&mut bus);
    cpu.y = 0xfa;

    cpu.run();
    assert_eq!(cpu.y, 0xfa); // didn't modify
    assert_eq!(cpu.p.contains(Status::ZERO), false);
    assert_eq!(cpu.p.contains(Status::CARRY), true);
    assert_eq!(cpu.p.contains(Status::NEGATIVE), true);
  }

  #[test]
  fn test_dec_eor() {
    let mut bus = vec![0xc6, 0x05, 0x45, 0x05, 0x00, 0x08];
    let mut cpu = Cpu::new(&mut bus);
    cpu.a = 0x03;

    cpu.run();
    assert_eq!(cpu.a, 0x04);
    assert_eq!(bus[5], 0x07);
  }
}
