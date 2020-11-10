use std::convert::TryInto;

use crate::bit::*;
use crate::status::*;

pub struct Cpu {
  /** The cartridge data */
  rom: [u8; 65536],

  /** The addressable memory */
  ram: [u8; 65536],

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
  pc: u16,

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
  s: u8,

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
  acc: u8,

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
  x: u8,

  /**
   * Index Register Y
   *
   * The Y register is similar to the X register in that it is available for
   * holding counter or offsets memory access and supports the same set of
   * memory load, save and compare operations as wells as increments and
   * decrements. It has no special functions.
   */
  y: u8,

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
  status: Status,
}

impl Cpu {
  fn new() -> Cpu {
    Cpu {
      rom: [0; 65536],
      ram: [0; 65536],
      pc: 0,
      s: 0xff,
      acc: 0,
      x: 0,
      y: 0,
      status: Status::new(),
    }
  }

  fn tick(&mut self) {
    // grab the instruction at `pc`
    // decode it, get operands, etc
    // perform whatever task it says
    // increment pc
    let op = self.rom[self.pc as usize];
    let mut pc_inc = 1;

    // start by matching opcodes without address modes
    match op {
      // TODO: figure out a more dry way
      0x10 => {
        self.bpl(self.rom[(self.pc + 1) as usize]);
        pc_inc = 2;
      }
      0x30 => {
        self.bmi(self.rom[(self.pc + 1) as usize]);
        pc_inc = 2;
      }
      0x50 => {
        self.bvc(self.rom[(self.pc + 1) as usize]);
        pc_inc = 2;
      }
      0x70 => {
        self.bvs(self.rom[(self.pc + 1) as usize]);
        pc_inc = 2;
      }
      0x90 => {
        self.bcc(self.rom[(self.pc + 1) as usize]);
        pc_inc = 2;
      }
      0xB0 => {
        self.bcs(self.rom[(self.pc + 1) as usize]);
        pc_inc = 2;
      }
      0xD0 => {
        self.bne(self.rom[(self.pc + 1) as usize]);
        pc_inc = 2;
      }
      0xF0 => {
        self.beq(self.rom[(self.pc + 1) as usize]);
        pc_inc = 2;
      }

      0x00 => self.brk(),
      0x20 => self.jsr(),
      0x40 => self.rti(),
      0x60 => self.rts(),

      0x08 => self.php(),
      0x28 => self.plp(),
      0x48 => self.pha(),
      0x68 => self.pla(),
      0x88 => self.dey(),
      0xA8 => self.tay(),
      0xC8 => self.iny(),
      0xE8 => self.inx(),

      0x18 => self.clc(),
      0x38 => self.sec(),
      0x58 => self.cli(),
      0x78 => self.sei(),
      0x98 => self.tya(),
      0xB8 => self.clv(),
      0xD8 => self.cld(),
      0xF8 => self.sed(),

      0x8A => self.txa(),
      0x9A => self.txs(),
      0xAA => self.tax(),
      0xBA => self.tsx(),
      0xCA => self.dex(),
      0xEA => self.nop(),

      _ => {
        // aaabbbcc
        let a = (op & 0b11100000) >> 5;
        let b = (op & 0b00011100) >> 2;
        let c = op & 0b00000011;

        if c == 0b01 {
          let val: u16;
          pc_inc = 2;
          match b {
            0b000 => {
              // (indirect,x)
              // @see http://obelisk.me.uk/6502/addressing.html#IDX
              let addr = self.rom[(self.pc + 1) as usize] + self.x;
              val = self.ram[addr as usize].into();
            }
            0b001 => {
              // zero page
              // @see http://obelisk.me.uk/6502/addressing.html#ZPG
              let addr = self.rom[(self.pc + 1) as usize];
              val = self.ram[addr as usize].into();
            }
            0b010 => {
              // #immediate
              // @see http://obelisk.me.uk/6502/addressing.html#IMM
              val = self.rom[(self.pc + 1) as usize].into();
            }
            0b011 => {
              // absolute
              // @see http://obelisk.me.uk/6502/addressing.html#ABS
              val = self.get_absolute_value();
              pc_inc = 3;
            }
            0b100 => {
              // (zero page),Y
              // @see http://obelisk.me.uk/6502/addressing.html#IDY
              let addr = self.rom[(self.pc + 1) as usize] + self.y;
              val = self.ram[addr as usize].into();
            }
            0b101 => {
              // zero page,X
              // @see http://obelisk.me.uk/6502/addressing.html#ZPX
              let addr = self.rom[(self.pc + 1) as usize] + self.x;
              val = self.ram[addr as usize].into();
            }
            0b110 => {
              // absolute,Y,
              // @see http://obelisk.me.uk/6502/addressing.html#ABY
              let addr: u16 = self.get_absolute_value() + self.y as u16;
              val = self.ram[addr as usize].into();
              pc_inc = 3;
            }
            0b111 => {
              // absolute,X,
              // @see http://obelisk.me.uk/6502/addressing.html#ABX
              let addr: u16 = self.get_absolute_value() + self.x as u16;
              val = self.ram[addr as usize].into();
              pc_inc = 3;
            }
          }

          match a {
            0b000 => self.ora(val.try_into().unwrap()),
            0b001 => self.and(val.try_into().unwrap()),
            0b010 => self.eor(val.try_into().unwrap()),
            0b011 => self.adc(val.try_into().unwrap()),
            0b100 => self.sta(val),
            0b101 => self.lda(val.try_into().unwrap()),
            0b110 => self.cmp(val.try_into().unwrap()),
            0b111 => self.sbc(val.try_into().unwrap()),
          }
          return;
        }

        let mem: &mut u8;
        match b {
          0b000 => {
            // #immediate
            mem = &mut self.rom[(self.pc + 1) as usize];
            pc_inc = 2;
          }
          0b001 => {
            // zero page
            let addr = self.rom[(self.pc + 1) as usize];
            mem = &mut self.ram[addr as usize];
            pc_inc = 2;
          }
          0b010 => {
            // accumulator
            mem = &mut self.acc;
          }
          0b011 => {
            // absolute
            mem = &mut self.ram[self.get_absolute_value() as usize];
            pc_inc = 3;
          }
          0b101 => {
            // zero page,X
            let offset = if c == 0b10 {
              match a {
                0b100 | 0b101 => self.y,
                _ => self.x,
              }
            } else {
              self.x
            };

            let addr = self.rom[(self.pc + 1) as usize] + offset;
            mem = &mut self.ram[addr as usize];
            pc_inc = 2;
          }
          0b111 => {
            // absolute,X
            let offset = if c == 0b10 {
              match a {
                0b100 | 0b101 => self.y,
                _ => self.x,
              }
            } else {
              self.x
            };

            let addr = self.get_absolute_value() + offset as u16;
            mem = &mut self.ram[addr as usize];
            pc_inc = 3;
          }
        }

        if c == 0b10 {
          match a {
            0b000 => self.asl(mem),
            0b001 => self.rol(mem),
            0b010 => self.lsr(mem),
            0b011 => self.ror(mem),
            0b100 => self.stx(mem),
            0b101 => self.ldx(mem),
            0b110 => self.dec(mem),
            0b111 => self.inc(mem),
          }

          return;
        }

        if c == 0b00 {
          match a {
            0b001 => self.bit(mem),
            0b010 => {
              // indirect
              let addr = self.get_absolute_value() as usize;
              self.jmp(get_u16(&self.ram[addr..2]));
            }
            0b011 => {
              // absolute
              ()
              // self.jmp(mem);
            }
            0b100 => self.sty(mem),
            0b101 => self.ldy(mem),
            0b110 => self.cpy(mem),
            0b111 => self.cpx(mem),
          }

          return;
        }
      }
    }

    self.pc += pc_inc;
  }

  fn push(&mut self, val: u8) {
    self.ram[(0x0100 & self.s) as usize] = val;
    self.s -= 1;
  }

  fn pull(&mut self) -> u8 {
    self.s += 1;
    self.ram[(0x0100 & self.s) as usize]
  }

  fn push_u16(&mut self, val: u16) {
    let (lo, hi) = split_u16(val);
    self.push(hi);
    self.push(lo);
  }

  fn pull_u16(&mut self) -> u16 {
    let lo = self.pull();
    let hi = self.pull();
    return make_u16(lo, hi);
  }

  // @see http://obelisk.me.uk/6502/addressing.html#ABS
  fn get_absolute_value(&self) -> u16 {
    get_u16(&self.rom[(self.pc + 1) as usize..2])
  }

  fn set_flags(&mut self, val: u8) {
    self.status.set_z(val == 0);
    self.status.set_n(check_bit(val, Bit::Seven));
  }

  // Load / Store Operations

  // first 3 set negative & zero
  fn lda(&mut self, val: u8) {
    self.acc = val;
    self.set_flags(self.acc);
  }

  fn ldx(&mut self, mem: &u8) {
    self.x = *mem;
    self.set_flags(self.x);
  }

  fn ldy(&mut self, mem: &u8) {
    self.y = *mem;
    self.set_flags(self.y);
  }

  fn sta(&mut self, val: u16) {
    self.ram[val as usize] = self.acc;
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
    self.set_flags(self.x);
  }

  fn txs(&mut self) {
    self.s = self.x;
  }

  fn pha(&mut self) {
    self.push(self.acc);
  }

  fn php(&mut self) {
    self.push(self.status.bits);
  }

  fn pla(&mut self) {
    self.acc = self.pull();
    self.set_flags(self.acc);
  }

  fn plp(&mut self) {
    self.status.bits = self.pull();
  }

  // Logical

  fn and(&mut self, val: u8) {
    self.acc &= val;
    self.set_flags(self.acc);
  }

  fn eor(&mut self, val: u8) {
    self.acc ^= val;
    self.set_flags(self.acc);
  }

  fn ora(&mut self, val: u8) {
    self.acc |= val;
    self.set_flags(self.acc);
  }

  fn bit(&mut self, mem: &mut u8) {
    self.status.set_z(self.acc & *mem == 0);
    self.status.set_v(check_bit(*mem, Bit::Six));
    self.status.set_n(check_bit(*mem, Bit::Seven));
  }

  // Arithmetic

  fn adc(&mut self, val: u8) {
    let initial_acc = self.acc;
    self.acc += val + (self.status.get_c() as u8);
    self.status.set_c(initial_acc > self.acc);
    self
      .status
      .set_v(self.status.get_c() ^ (check_bit(initial_acc, Bit::Seven)));
    self.set_flags(self.acc);
  }

  fn sbc(&mut self, val: u8) {
    let initial_acc = self.acc;
    self.acc = self.acc - val - (1 - self.status.get_c() as u8);
    self.status.set_c(initial_acc < self.acc);
    self
      .status
      .set_v(self.status.get_c() as u8 ^ (check_bit(initial_acc, Bit::Seven) as u8) != 0);
    self.set_flags(self.acc);
  }

  fn cmp(&mut self, val: u8) {
    self.status.set_c(self.acc >= val);
    self.status.set_z(self.acc == val);
    self.status.set_n(self.acc > (Bit::Seven as u8));
  }

  // N,Z,C
  fn cpx(&mut self, mem: &u8) {}
  // N,Z,C
  fn cpy(&mut self, mem: &u8) {}

  // Increments & Decrements

  fn inc(&mut self, mem: &u8) {
    *mem += 1;
    self.set_flags(*mem);
  }

  fn inx(&mut self) {
    self.x += 1;
    self.set_flags(self.x)
  }

  fn iny(&mut self) {
    self.y += 1;
    self.set_flags(self.y)
  }

  fn dec(&mut self, mem: &u8) {
    *mem -= 1;
    self.set_flags(*mem);
  }

  fn dex(&mut self) {
    self.x -= 1;
    self.set_flags(self.x)
  }

  fn dey(&mut self) {
    self.y -= 1;
    self.set_flags(self.y)
  }

  // Shifts

  fn asl(&mut self, addr: &mut u8) {
    self.status.set_c(check_bit(*addr, Bit::Seven));
    *addr <<= 1;
    self.set_flags(*addr);
  }

  fn lsr(&mut self, addr: &mut u8) {
    self.status.set_c(check_bit(*addr, Bit::Zero));
    *addr >>= 1;
    self.set_flags(*addr);
  }

  fn rol(&mut self, addr: &mut u8) {
    // Rotate Left 	N,Z,C
    let carry = self.status.get_c() as u8;
    self.status.set_c(check_bit(*addr, Bit::Seven));
    *addr <<= 1;
    *addr |= carry;
    self.set_flags(*addr);
  }

  fn ror(&mut self, addr: &mut u8) {
    let carry = self.status.get_c() as u8;
    self.status.set_c(check_bit(*addr, Bit::Zero));
    *addr >>= 1;
    *addr |= carry << 7;
    self.set_flags(*addr);
  }

  // Jumps & Calls

  fn jmp(&mut self, addr: u16) {
    self.pc = addr;
  }

  fn jsr(&mut self) {
    // Jump to a subroutine
    self.push_u16(self.pc + 2);
    self.pc = self.get_absolute_value();
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

    let lo = self.ram[0xfffe];
    let hi = self.ram[0xffff];
    self.pc = make_u16(lo, hi);

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
