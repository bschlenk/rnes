use crate::bit::add_signed;
use crate::cpu::{
  AddressMode::{self, *},
  Cpu,
};
use std::fmt;

#[derive(Debug)]
pub enum OpCode {
  ADC,
  AND,
  ASL,
  BCC,
  BCS,
  BEQ,
  BIT,
  BMI,
  BNE,
  BPL,
  BRK,
  BVC,
  BVS,
  CLC,
  CLD,
  CLI,
  CLV,
  CMP,
  CPX,
  CPY,
  DEC,
  DEX,
  DEY,
  EOR,
  INC,
  INX,
  INY,
  JMP,
  JSR,
  LDA,
  LDX,
  LDY,
  LSR,
  NOP,
  ORA,
  PHA,
  PHP,
  PLA,
  PLP,
  ROL,
  ROR,
  RTI,
  RTS,
  SBC,
  SEC,
  SED,
  SEI,
  STA,
  STX,
  STY,
  TAX,
  TAY,
  TSX,
  TXA,
  TXS,
  TYA,
  XXX, // sigil for bad opcode
  // Unofficial Opcodes
  AAC,
  DCP,
  ISB,
  LAX,
  RLA,
  RRA,
  SAX,
  SLO,
  SRE,
}

use OpCode::*;

pub struct OpInfo {
  pub id: u8,
  pub op: OpCode,
  pub len: u8,
  pub cycles: u8,
  pub mode: AddressMode,
  pub official: bool,
}

impl fmt::Debug for OpInfo {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "OpInfo {{ id: 0x{:02x}, op: {:?}, mode: {:?} }}",
      self.id, self.op, self.mode
    )
  }
}

impl OpInfo {
  pub fn new(id: u8, op: OpCode, len: u8, cycles: u8, mode: AddressMode, official: bool) -> Self {
    Self {
      id,
      op,
      len,
      cycles,
      mode,
      official,
    }
  }

  pub fn to_assembly(&self, cpu: &mut Cpu) -> String {
    let op = match self.op {
      BPL | BMI | BVC | BVS | BCC | BCS | BNE | BEQ => {
        // print where we're going instead of offset
        format!("${:04X}", 1 + add_signed(cpu.pc, cpu.read_pc()))
      }
      _ => match self.mode {
        Implicit => "".to_string(),
        Accumulator => "A".to_string(),
        Immediate => format!("#${:02X}", cpu.read_pc()),
        ZeroPage => {
          let addr = cpu.read_pc();
          format!("${:02X} = {:02X}", addr, cpu.read(addr as u16))
        }
        ZeroPageX => {
          let addr = cpu.get_operand_address(&ZeroPageX);
          let val = cpu.read(addr);
          format!("${:02X},X @ {:02X} = {:02X}", cpu.read_pc(), addr, val)
        }
        ZeroPageY => {
          let addr = cpu.get_operand_address(&ZeroPageY);
          let val = cpu.read(addr);
          format!("${:02X},Y @ {:02X} = {:02X}", cpu.read_pc(), addr, val)
        }
        Absolute => match self.op {
          JMP | JSR => format!("${:04X}", cpu.read_pc_u16()),
          _ => {
            let addr = cpu.read_pc_u16();
            format!("${:04X} = {:02X}", addr, cpu.read(addr))
          }
        },
        AbsoluteX => {
          let addr = cpu.get_operand_address(&AbsoluteX);
          let val = cpu.read(addr);
          format!("${:04X},X @ {:04X} = {:02X}", cpu.read_pc_u16(), addr, val)
        }
        AbsoluteY => {
          let addr = cpu.get_operand_address(&AbsoluteY);
          let val = cpu.read(addr);
          format!("${:04X},Y @ {:04X} = {:02X}", cpu.read_pc_u16(), addr, val)
        }
        Indirect => format!(
          "(${:04X}) = {:04X}",
          cpu.read_pc_u16(),
          cpu.get_operand_address(&Indirect)
        ),
        IndirectX => {
          let pc = cpu.read_pc();
          let addr = pc.wrapping_add(cpu.x);
          let target = cpu.get_operand_address(&IndirectX);
          let val = cpu.read(target);
          format!(
            "(${:02X},X) @ {:02X} = {:04X} = {:02X}",
            cpu.read_pc(),
            addr,
            target,
            val
          )
        }
        IndirectY => {
          let pc = cpu.read_pc();
          let addr = cpu.bus.wrapping_read_u16(pc);
          let addrp = addr.wrapping_add(cpu.y as u16);
          let val = cpu.read(addrp);
          format!(
            "(${:02X}),Y = {:04X} @ {:04X} = {:02X}",
            pc, addr, addrp, val
          )
        }
      },
    };

    format!(
      "{}{:?} {}",
      if self.official { " " } else { "*" },
      self.op,
      op
    )
  }
}

lazy_static! {
  pub static ref BAD_OPCODE: OpInfo = OpInfo::new(0x00, XXX, 0, 0, Implicit, false);
  pub static ref OPCODES: Vec<OpInfo> = vec![
    OpInfo::new(0x00, BRK, 1, 7, Implicit, true),
    OpInfo::new(0xea, NOP, 1, 2, Implicit, true),
    OpInfo::new(0x40, RTI, 1, 6, Implicit, true),

    // Register Instructions

    OpInfo::new(0xaa, TAX, 1, 2, Implicit, true),
    OpInfo::new(0x8a, TXA, 1, 2, Implicit, true),
    OpInfo::new(0xca, DEX, 1, 2, Implicit, true),
    OpInfo::new(0xe8, INX, 1, 2, Implicit, true),
    OpInfo::new(0xa8, TAY, 1, 2, Implicit, true),
    OpInfo::new(0x98, TYA, 1, 2, Implicit, true),
    OpInfo::new(0x88, DEY, 1, 2, Implicit, true),
    OpInfo::new(0xc8, INY, 1, 2, Implicit, true),

    // Stack pointer instructions

    OpInfo::new(0x9a, TXS, 1, 2, Implicit, true),
    OpInfo::new(0xba, TSX, 1, 2, Implicit, true),
    OpInfo::new(0x48, PHA, 1, 3, Implicit, true),
    OpInfo::new(0x68, PLA, 1, 4, Implicit, true),
    OpInfo::new(0x08, PHP, 1, 3, Implicit, true),
    OpInfo::new(0x28, PLP, 1, 4, Implicit, true),

    OpInfo::new(0x69, ADC, 2, 2, Immediate, true),
    OpInfo::new(0x65, ADC, 2, 3, ZeroPage, true),
    OpInfo::new(0x75, ADC, 2, 4, ZeroPageX, true),
    OpInfo::new(0x6d, ADC, 3, 4, Absolute, true),
    OpInfo::new(0x7d, ADC, 3, 4/*+ add 1 cycle if page boundary crossed*/, AbsoluteX, true),
    OpInfo::new(0x79, ADC, 3, 4/*+ add 1 cycle if page boundary crossed*/, AbsoluteY, true),
    OpInfo::new(0x61, ADC, 2, 6, IndirectX, true),
    OpInfo::new(0x71, ADC, 2, 5/*+ add 1 cycle if page boundary crossed*/, IndirectY, true),

    OpInfo::new(0xe9, SBC, 2, 2, Immediate, true),
    OpInfo::new(0xe5, SBC, 2, 3, ZeroPage, true),
    OpInfo::new(0xf5, SBC, 2, 4, ZeroPageX, true),
    OpInfo::new(0xed, SBC, 3, 4, Absolute, true),
    OpInfo::new(0xfd, SBC, 3, 4/*+ add 1 cycle if page boundary crossed*/, AbsoluteX, true),
    OpInfo::new(0xf9, SBC, 3, 4/*+ add 1 cycle if page boundary crossed*/, AbsoluteY, true),
    OpInfo::new(0xe1, SBC, 2, 6, IndirectX, true),
    OpInfo::new(0xf1, SBC, 2, 5/*+ add 1 cycle if page boundary crossed*/, IndirectY, true),

    OpInfo::new(0x29, AND, 2, 2, Immediate, true),
    OpInfo::new(0x25, AND, 2, 3, ZeroPage, true),
    OpInfo::new(0x35, AND, 2, 4, ZeroPageX, true),
    OpInfo::new(0x2d, AND, 3, 4, Absolute, true),
    OpInfo::new(0x3d, AND, 3, 4/*+ add 1 cycle if page boundary crossed*/, AbsoluteX, true),
    OpInfo::new(0x39, AND, 3, 4/*+ add 1 cycle if page boundary crossed*/, AbsoluteY, true),
    OpInfo::new(0x21, AND, 2, 6, IndirectX, true),
    OpInfo::new(0x31, AND, 2, 5/*+ add 1 cycle if page boundary crossed*/, IndirectY, true),

    OpInfo::new(0x09, ORA, 2, 2, Immediate, true),
    OpInfo::new(0x05, ORA, 2, 3, ZeroPage, true),
    OpInfo::new(0x15, ORA, 2, 4, ZeroPageX, true),
    OpInfo::new(0x0d, ORA, 3, 4, Absolute, true),
    OpInfo::new(0x1d, ORA, 3, 4/*+ add 1 cycle if page boundary crossed*/, AbsoluteX, true),
    OpInfo::new(0x19, ORA, 3, 4/*+ add 1 cycle if page boundary crossed*/, AbsoluteY, true),
    OpInfo::new(0x01, ORA, 2, 6, IndirectX, true),
    OpInfo::new(0x11, ORA, 2, 5/*+ add 1 cycle if page boundary crossed*/, IndirectY, true),

    OpInfo::new(0x0a, ASL, 1, 2, Accumulator, true),
    OpInfo::new(0x06, ASL, 2, 5, ZeroPage, true),
    OpInfo::new(0x16, ASL, 2, 6, ZeroPageX, true),
    OpInfo::new(0x0e, ASL, 3, 6, Absolute, true),
    OpInfo::new(0x1e, ASL, 3, 7, AbsoluteX, true),

    OpInfo::new(0x4a, LSR, 1, 2, Accumulator, true),
    OpInfo::new(0x46, LSR, 2, 5, ZeroPage, true),
    OpInfo::new(0x56, LSR, 2, 6, ZeroPageX, true),
    OpInfo::new(0x4e, LSR, 3, 6, Absolute, true),
    OpInfo::new(0x5e, LSR, 3, 7, AbsoluteX, true),

    OpInfo::new(0x2a, ROL, 1, 2, Accumulator, true),
    OpInfo::new(0x26, ROL, 2, 5, ZeroPage, true),
    OpInfo::new(0x36, ROL, 2, 6, ZeroPageX, true),
    OpInfo::new(0x2e, ROL, 3, 6, Absolute, true),
    OpInfo::new(0x3e, ROL, 3, 7, AbsoluteX, true),

    OpInfo::new(0x6a, ROR, 1, 2, Accumulator, true),
    OpInfo::new(0x66, ROR, 2, 5, ZeroPage, true),
    OpInfo::new(0x76, ROR, 2, 6, ZeroPageX, true),
    OpInfo::new(0x6e, ROR, 3, 6, Absolute, true),
    OpInfo::new(0x7e, ROR, 3, 7, AbsoluteX, true),

    OpInfo::new(0x24, BIT, 2, 3, ZeroPage, true),
    OpInfo::new(0x2c, BIT, 3, 4, Absolute, true),

    // Branching Instructions

    // Branches are dependant on the status of the flag bits when the op code is
    // encountered. A branch not taken requires two machine cycles. Add one if
    // the branch is taken and add one more if the branch crosses a page boundary.

    OpInfo::new(0x10, BPL, 2, 2, Immediate, true),
    OpInfo::new(0x30, BMI, 2, 2, Immediate, true),
    OpInfo::new(0x50, BVC, 2, 2, Immediate, true),
    OpInfo::new(0x70, BVS, 2, 2, Immediate, true),
    OpInfo::new(0x90, BCC, 2, 2, Immediate, true),
    OpInfo::new(0xb0, BCS, 2, 2, Immediate, true),
    OpInfo::new(0xd0, BNE, 2, 2, Immediate, true),
    OpInfo::new(0xf0, BEQ, 2, 2, Immediate, true),

    OpInfo::new(0x4c, JMP, 3, 3, Absolute, true),
    OpInfo::new(0x6c, JMP, 3, 5, Indirect, true),

    OpInfo::new(0x20, JSR, 3, 6, Absolute, true),
    OpInfo::new(0x60, RTS, 1, 6, Implicit, true),

    OpInfo::new(0xc9, CMP, 2, 2, Immediate, true),
    OpInfo::new(0xc5, CMP, 2, 3, ZeroPage, true),
    OpInfo::new(0xd5, CMP, 2, 4, ZeroPageX, true),
    OpInfo::new(0xcd, CMP, 3, 4, Absolute, true),
    OpInfo::new(0xdd, CMP, 3, 4/*+ add 1 cycle if page boundary crossed*/, AbsoluteX, true),
    OpInfo::new(0xd9, CMP, 3, 4/*+ add 1 cycle if page boundary crossed*/, AbsoluteY, true),
    OpInfo::new(0xc1, CMP, 2, 6, IndirectX, true),
    OpInfo::new(0xd1, CMP, 2, 5/*+ add 1 cycle if page boundary crossed*/, IndirectY, true),

    OpInfo::new(0xe0, CPX, 2, 2, Immediate, true),
    OpInfo::new(0xe4, CPX, 2, 3, ZeroPage, true),
    OpInfo::new(0xec, CPX, 3, 4, Absolute, true),

    OpInfo::new(0xc0, CPY, 2, 2, Immediate, true),
    OpInfo::new(0xc4, CPY, 2, 3, ZeroPage, true),
    OpInfo::new(0xcc, CPY, 3, 4, Absolute, true),

    OpInfo::new(0xc6, DEC, 2, 5, ZeroPage, true),
    OpInfo::new(0xd6, DEC, 2, 6, ZeroPageX, true),
    OpInfo::new(0xce, DEC, 3, 6, Absolute, true),
    OpInfo::new(0xde, DEC, 3, 7, AbsoluteX, true),

    OpInfo::new(0xe6, INC, 2, 5, ZeroPage, true),
    OpInfo::new(0xf6, INC, 2, 6, ZeroPageX, true),
    OpInfo::new(0xee, INC, 3, 6, Absolute, true),
    OpInfo::new(0xfe, INC, 3, 7, AbsoluteX, true),

    OpInfo::new(0x49, EOR, 2, 2, Immediate, true),
    OpInfo::new(0x45, EOR, 2, 3, ZeroPage, true),
    OpInfo::new(0x55, EOR, 2, 4, ZeroPageX, true),
    OpInfo::new(0x4D, EOR, 3, 4, Absolute, true),
    OpInfo::new(0x5D, EOR, 3, 4/*+ add 1 cycle if page boundary crossed*/, AbsoluteX, true),
    OpInfo::new(0x59, EOR, 3, 4/*+ add 1 cycle if page boundary crossed*/, AbsoluteY, true),
    OpInfo::new(0x41, EOR, 2, 6, IndirectX, true),
    OpInfo::new(0x51, EOR, 2, 5/*+ add 1 cycle if page boundary crossed*/, IndirectY, true),

    // Flag (Processor Status) Instructions

    OpInfo::new(0x18, CLC, 1, 2, Implicit, true),
    OpInfo::new(0x38, SEC, 1, 2, Implicit, true),
    OpInfo::new(0x58, CLI, 1, 2, Implicit, true),
    OpInfo::new(0x78, SEI, 1, 2, Implicit, true),
    OpInfo::new(0xb8, CLV, 1, 2, Implicit, true),
    OpInfo::new(0xd8, CLD, 1, 2, Implicit, true),
    OpInfo::new(0xf8, SED, 1, 2, Implicit, true),

    // Load/Store Registers

    OpInfo::new(0xa9, LDA, 2, 2, Immediate, true),
    OpInfo::new(0xa5, LDA, 2, 3, ZeroPage, true),
    OpInfo::new(0xb5, LDA, 2, 4, ZeroPageX, true),
    OpInfo::new(0xad, LDA, 3, 4, Absolute, true),
    OpInfo::new(0xbd, LDA, 3, 4/*+ add 1 cycle if page boundary crossed*/, AbsoluteX, true),
    OpInfo::new(0xb9, LDA, 3, 4/*+ add 1 cycle if page boundary crossed*/, AbsoluteY, true),
    OpInfo::new(0xa1, LDA, 2, 6, IndirectX, true),
    OpInfo::new(0xb1, LDA, 2, 5/*+ add 1 cycle if page boundary crossed*/, IndirectY, true),

    OpInfo::new(0xa2, LDX, 2, 2, Immediate, true),
    OpInfo::new(0xa6, LDX, 2, 3, ZeroPage, true),
    OpInfo::new(0xb6, LDX, 2, 4, ZeroPageY, true),
    OpInfo::new(0xae, LDX, 3, 4, Absolute, true),
    OpInfo::new(0xbe, LDX, 3, 4/*+ add 1 cycle if page boundary crossed*/, AbsoluteY, true),

    OpInfo::new(0xa0, LDY, 2, 2, Immediate, true),
    OpInfo::new(0xa4, LDY, 2, 3, ZeroPage, true),
    OpInfo::new(0xb4, LDY, 2, 4, ZeroPageX, true),
    OpInfo::new(0xac, LDY, 3, 4, Absolute, true),
    OpInfo::new(0xbc, LDY, 3, 4/*+ add 1 cycle if page boundary crossed*/, AbsoluteX, true),

    OpInfo::new(0x85, STA, 2, 3, ZeroPage, true),
    OpInfo::new(0x95, STA, 2, 4, ZeroPageX, true),
    OpInfo::new(0x8d, STA, 3, 4, Absolute, true),
    OpInfo::new(0x9d, STA, 3, 5, AbsoluteX, true),
    OpInfo::new(0x99, STA, 3, 5, AbsoluteY, true),
    OpInfo::new(0x81, STA, 2, 6, IndirectX, true),
    OpInfo::new(0x91, STA, 2, 6, IndirectY, true),

    OpInfo::new(0x86, STX, 2, 3, ZeroPage, true),
    OpInfo::new(0x96, STX, 2, 4, ZeroPageY, true),
    OpInfo::new(0x8e, STX, 3, 4, Absolute, true),

    OpInfo::new(0x84, STY, 2, 3, ZeroPage, true),
    OpInfo::new(0x94, STY, 2, 4, ZeroPageX, true),
    OpInfo::new(0x8c, STY, 3, 4, Absolute, true),

    // Unofficial Opcodes

    OpInfo::new(0x0b, AAC, 2, 2, Immediate, false),
    OpInfo::new(0x2b, AAC, 2, 2, Immediate, false),

    /*
    AAX (SAX) [AXS]
=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D
AND X register with accumulator and store result in memory. Status
flags: N,Z

Addressing  |Mnemonics  |Opc|Sz | n
------------|-----------|---|---|---
Zero Page   |AAX arg    |$87| 2 | 3
Zero Page,Y |AAX arg,Y  |$97| 2 | 4
(Indirect,X)|AAX (arg,X)|$83| 2 | 6
Absolute    |AAX arg    |$8F| 3 | 4


ARR (ARR) [ARR]
=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D
AND byte with accumulator, then rotate one bit right in accu-mulator and
check bit 5 and 6:
If both bits are 1: set C, clear V.
If both bits are 0: clear C and V.
If only bit 5 is 1: set V, clear C.
If only bit 6 is 1: set C and V.
Status flags: N,V,Z,C

Addressing  |Mnemonics  |Opc|Sz | n
------------|-----------|---|---|---
Immediate   |ARR #arg   |$6B| 2 | 2

ASR (ASR) [ALR]
=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D
AND byte with accumulator, then shift right one bit in accumu-lator.
Status flags: N,Z,C

Addressing  |Mnemonics  |Opc|Sz | n
------------|-----------|---|---|---
Immediate   |ASR #arg   |$4B| 2 | 2

ATX (LXA) [OAL]
=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D
AND byte with accumulator, then transfer accumulator to X register.
Status flags: N,Z

Addressing  |Mnemonics  |Opc|Sz | n
------------|-----------|---|---|---
Immediate   |ATX #arg   |$AB| 2 | 2

AXA (SHA) [AXA]
=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D
AND X register with accumulator then AND result with 7 and store in
memory. Status flags: -

Addressing  |Mnemonics  |Opc|Sz | n
------------|-----------|---|---|---
Absolute,Y  |AXA arg,Y  |$9F| 3 | 5
(Indirect),Y|AXA arg    |$93| 2 | 6


AXS (SBX) [SAX]
=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D
AND X register with accumulator and store result in X regis-ter, then
subtract byte from X register (without borrow).
Status flags: N,Z,C

Addressing  |Mnemonics  |Opc|Sz | n
------------|-----------|---|---|---
Immediate   |AXS #arg   |$CB| 2 | 2
*/

    OpInfo::new(0xc7, DCP, 2, 5, ZeroPage, false),
    OpInfo::new(0xd7, DCP, 2, 6, ZeroPageX, false),
    OpInfo::new(0xcf, DCP, 3, 6, Absolute, false),
    OpInfo::new(0xdf, DCP, 3, 7, AbsoluteX, false),
    OpInfo::new(0xdb, DCP, 3, 7, AbsoluteY, false),
    OpInfo::new(0xc3, DCP, 2, 8, IndirectX, false),
    OpInfo::new(0xd3, DCP, 2, 8, IndirectY, false),

    OpInfo::new(0xe7, ISB, 2, 5, ZeroPage, false),
    OpInfo::new(0xf7, ISB, 2, 6, ZeroPageX, false),
    OpInfo::new(0xef, ISB, 3, 6, Absolute, false),
    OpInfo::new(0xff, ISB, 3, 7, AbsoluteX, false),
    OpInfo::new(0xfb, ISB, 3, 7, AbsoluteY, false),
    OpInfo::new(0xe3, ISB, 2, 8, IndirectX, false),
    OpInfo::new(0xf3, ISB, 2, 8, IndirectY, false),

    /*
    LAR (LAE) [LAS]
=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D
AND memory with stack pointer, transfer result to accu-mulator, X
register and stack pointer.
Status flags: N,Z

Addressing  |Mnemonics  |Opc|Sz | n
------------|-----------|---|---|---
Absolute,Y  |LAR arg,Y  |$BB| 3 | 4 *
*/

    OpInfo::new(0xa7, LAX, 2, 3, ZeroPage, false),
    OpInfo::new(0xb7, LAX, 2, 4, ZeroPageY, false),
    OpInfo::new(0xaf, LAX, 3, 4, Absolute, false),
    OpInfo::new(0xbf, LAX, 3, 4/*+1 for page crossed*/, AbsoluteY, false),
    OpInfo::new(0xa3, LAX, 2, 6, IndirectX, false),
    OpInfo::new(0xb3, LAX, 2, 5/*+1 for page crossed*/, IndirectY, false),

    OpInfo::new(0x27, RLA, 2, 5, ZeroPage, false),
    OpInfo::new(0x37, RLA, 2, 6, ZeroPageX, false),
    OpInfo::new(0x2f, RLA, 3, 6, Absolute, false),
    OpInfo::new(0x3f, RLA, 3, 7, AbsoluteX, false),
    OpInfo::new(0x3b, RLA, 3, 7, AbsoluteY, false),
    OpInfo::new(0x23, RLA, 2, 8, IndirectX, false),
    OpInfo::new(0x33, RLA, 2, 8, IndirectY, false),

    OpInfo::new(0x67, RRA, 2, 5, ZeroPage, false),
    OpInfo::new(0x77, RRA, 2, 6, ZeroPageX, false),
    OpInfo::new(0x6f, RRA, 3, 6, Absolute, false),
    OpInfo::new(0x7f, RRA, 3, 7, AbsoluteX, false),
    OpInfo::new(0x7b, RRA, 3, 7, AbsoluteY, false),
    OpInfo::new(0x63, RRA, 2, 8, IndirectX, false),
    OpInfo::new(0x73, RRA, 2, 8, IndirectY, false),

    OpInfo::new(0x87, SAX, 2, 3, ZeroPage, false),
    OpInfo::new(0x97, SAX, 2, 4, ZeroPageY, false),
    OpInfo::new(0x83, SAX, 2, 6, IndirectX, false),
    OpInfo::new(0x8f, SAX, 3, 4, Absolute, false),

    OpInfo::new(0xeb, SBC, 2, 2, Immediate, false),

    OpInfo::new(0x07, SLO, 2, 5, ZeroPage, false),
    OpInfo::new(0x17, SLO, 2, 6, ZeroPageX, false),
    OpInfo::new(0x0f, SLO, 3, 6, Absolute, false),
    OpInfo::new(0x1f, SLO, 3, 7, AbsoluteX, false),
    OpInfo::new(0x1b, SLO, 3, 7, AbsoluteY, false),
    OpInfo::new(0x03, SLO, 2, 8, IndirectX, false),
    OpInfo::new(0x13, SLO, 2, 8, IndirectY, false),

    OpInfo::new(0x47, SRE, 2, 5, ZeroPage, false),
    OpInfo::new(0x57, SRE, 2, 6, ZeroPageX, false),
    OpInfo::new(0x4f, SRE, 3, 6, Absolute, false),
    OpInfo::new(0x5f, SRE, 3, 7, AbsoluteX, false),
    OpInfo::new(0x5b, SRE, 3, 7, AbsoluteY, false),
    OpInfo::new(0x43, SRE, 2, 8, IndirectX, false),
    OpInfo::new(0x53, SRE, 2, 8, IndirectY, false),

    /*
    SXA (SHX) [XAS]
=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D
AND X register with the high byte of the target address of the argument
+ 1. Store the result in memory.

M =3D X AND HIGH(arg) + 1

Status flags: -

Addressing  |Mnemonics  |Opc|Sz | n
------------|-----------|---|---|---
Absolute,Y  |SXA arg,Y  |$9E| 3 | 5

SYA (SHY) [SAY]
=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D
AND Y register with the high byte of the target address of the argument
+ 1. Store the result in memory.

M =3D Y AND HIGH(arg) + 1

Status flags: -

Addressing  |Mnemonics  |Opc|Sz | n
------------|-----------|---|---|---
Absolute,X  |SYA arg,X  |$9C| 3 | 5

XAA (ANE) [XAA]
=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D
Exact operation unknown. Read the referenced documents for more
information and observations.

Addressing  |Mnemonics  |Opc|Sz | n
------------|-----------|---|---|---
Immediate   |XAA #arg   |$8B| 2 | 2


XAS (SHS) [TAS]
=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D
AND X register with accumulator and store result in stack pointer, then
AND stack pointer with the high byte of the target address of the
argument + 1. Store result in memory.

S =3D X AND A, M =3D S AND HIGH(arg) + 1

Status flags: -

Addressing  |Mnemonics  |Opc|Sz | n
------------|-----------|---|---|---
Absolute,Y  |XAS arg,Y  |$9B| 3 | 5
    */

    OpInfo::new(0x1a, NOP, 1, 2, Implicit, false),
    OpInfo::new(0x3a, NOP, 1, 2, Implicit, false),
    OpInfo::new(0x5a, NOP, 1, 2, Implicit, false),
    OpInfo::new(0x7a, NOP, 1, 2, Implicit, false),
    OpInfo::new(0xda, NOP, 1, 2, Implicit, false),
    OpInfo::new(0xfa, NOP, 1, 2, Implicit, false),

    // Double NOP

    OpInfo::new(0x04, NOP, 2, 3, ZeroPage, false),
    OpInfo::new(0x14, NOP, 2, 4, ZeroPageX, false),
    OpInfo::new(0x34, NOP, 2, 4, ZeroPageX, false),
    OpInfo::new(0x44, NOP, 2, 3, ZeroPage, false),
    OpInfo::new(0x54, NOP, 2, 4, ZeroPageX, false),
    OpInfo::new(0x64, NOP, 2, 3, ZeroPage, false),
    OpInfo::new(0x74, NOP, 2, 4, ZeroPageX, false),
    OpInfo::new(0x80, NOP, 2, 2, Immediate, false),
    OpInfo::new(0x82, NOP, 2, 2, Immediate, false),
    OpInfo::new(0x89, NOP, 2, 2, Immediate, false),
    OpInfo::new(0xc2, NOP, 2, 2, Immediate, false),
    OpInfo::new(0xd4, NOP, 2, 4, ZeroPageX, false),
    OpInfo::new(0xe2, NOP, 2, 2, Immediate, false),
    OpInfo::new(0xf4, NOP, 2, 4, ZeroPageX, false),

    // Triple NOP

    OpInfo::new(0x0c, NOP, 3, 4, Absolute, false),
    OpInfo::new(0x1c, NOP, 3, 4/*+1 if page crossed*/, AbsoluteX, false),
    OpInfo::new(0x3c, NOP, 3, 4/*+1 if page crossed*/, AbsoluteX, false),
    OpInfo::new(0x5c, NOP, 3, 4/*+1 if page crossed*/, AbsoluteX, false),
    OpInfo::new(0x7c, NOP, 3, 4/*+1 if page crossed*/, AbsoluteX, false),
    OpInfo::new(0xdc, NOP, 3, 4/*+1 if page crossed*/, AbsoluteX, false),
    OpInfo::new(0xfc, NOP, 3, 4/*+1 if page crossed*/, AbsoluteX, false),

    /*
    KIL (JAM) [HLT]
=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D
Stop program counter (processor lock up).
Status flags: -

Addressing  |Mnemonics  |Opc|Sz | n
------------|-----------|---|---|---
Implied     |KIL        |$02| 1 | -
Implied     |KIL        |$12| 1 | -
Implied     |KIL        |$22| 1 | -
Implied     |KIL        |$32| 1 | -
Implied     |KIL        |$42| 1 | -
Implied     |KIL        |$52| 1 | -
Implied     |KIL        |$62| 1 | -
Implied     |KIL        |$72| 1 | -
Implied     |KIL        |$92| 1 | -
Implied     |KIL        |$B2| 1 | -
Implied     |KIL        |$D2| 1 | -
Implied     |KIL        |$F2| 1 | -
*/
  ];
  pub static ref OPCODES_MAP: [&'static OpInfo; 0x100] = {
    let mut map: [&'static OpInfo; 0x100] = [&BAD_OPCODE; 0x100];
    for op in OPCODES.iter() {
      map[op.id as usize] = op;
    }
    map
  };
}
