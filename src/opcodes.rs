use crate::cpu::AddressMode::{self, *};

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
}

use OpCode::*;

#[derive(Debug)]
pub struct OpInfo {
  pub id: u8,
  pub op: OpCode,
  pub len: u8,
  pub cycles: u8,
  pub mode: AddressMode,
}

impl OpInfo {
  pub fn new(id: u8, op: OpCode, len: u8, cycles: u8, mode: AddressMode) -> Self {
    Self {
      id,
      op,
      len,
      cycles,
      mode,
    }
  }
}

lazy_static! {
  pub static ref BAD_OPCODE: OpInfo = OpInfo::new(0x00, XXX, 0, 0, Implicit);
  pub static ref OPCODES: Vec<OpInfo> = vec![
    OpInfo::new(0x00, BRK, 1, 7, Implicit),

    OpInfo::new(0xaa, TAX, 1, 2, Implicit),
    OpInfo::new(0x8a, TXA, 1, 2, Implicit),
    OpInfo::new(0xca, DEX, 1, 2, Implicit),
    OpInfo::new(0xe8, INX, 1, 2, Implicit),
    OpInfo::new(0xa8, TAY, 1, 2, Implicit),
    OpInfo::new(0x98, TYA, 1, 2, Implicit),
    OpInfo::new(0x88, DEY, 1, 2, Implicit),
    OpInfo::new(0xc8, INY, 1, 2, Implicit),

    OpInfo::new(0x69, ADC, 2, 2, Immediate),
    OpInfo::new(0x65, ADC, 2, 3, ZeroPage),
    OpInfo::new(0x75, ADC, 2, 4, ZeroPageX),
    OpInfo::new(0x6d, ADC, 3, 4, Absolute),
    OpInfo::new(0x7d, ADC, 3, 4/*+ add 1 cycle if page boundary crossed*/, AbsoluteX),
    OpInfo::new(0x79, ADC, 3, 4/*+ add 1 cycle if page boundary crossed*/, AbsoluteY),
    OpInfo::new(0x61, ADC, 2, 6, IndirectX),
    OpInfo::new(0x71, ADC, 2, 5/*+ add 1 cycle if page boundary crossed*/, IndirectY),

    OpInfo::new(0x29, AND, 2, 2, Immediate),
    OpInfo::new(0x25, AND, 2, 3, ZeroPage),
    OpInfo::new(0x35, AND, 2, 4, ZeroPageX),
    OpInfo::new(0x2d, AND, 3, 4, Absolute),
    OpInfo::new(0x3d, AND, 3, 4/*+ add 1 cycle if page boundary crossed*/, AbsoluteX),
    OpInfo::new(0x39, AND, 3, 4/*+ add 1 cycle if page boundary crossed*/, AbsoluteY),
    OpInfo::new(0x21, AND, 2, 6, IndirectX),
    OpInfo::new(0x31, AND, 2, 5/*+ add 1 cycle if page boundary crossed*/, IndirectY),

    OpInfo::new(0x0a, ASL, 1, 2, Implicit), /* accumulator */
    OpInfo::new(0x06, ASL, 2, 5, ZeroPage),
    OpInfo::new(0x16, ASL, 2, 6, ZeroPageX),
    OpInfo::new(0x0e, ASL, 3, 6, Absolute),
    OpInfo::new(0x1e, ASL, 3, 7, AbsoluteX),

    OpInfo::new(0x24, BIT, 2, 3, ZeroPage),
    OpInfo::new(0x2c, BIT, 3, 4, Absolute),

    OpInfo::new(0x85, STA, 2, 3, ZeroPage),
    OpInfo::new(0x95, STA, 2, 4, ZeroPageX),
    OpInfo::new(0x8d, STA, 3, 4, Absolute),
    OpInfo::new(0x9d, STA, 3, 5, AbsoluteX),
    OpInfo::new(0x99, STA, 3, 5, AbsoluteY),
    OpInfo::new(0x81, STA, 2, 6, IndirectX),
    OpInfo::new(0x91, STA, 2, 6, IndirectY),

    OpInfo::new(0xa9, LDA, 2, 2, Immediate),
    OpInfo::new(0xa5, LDA, 2, 3, ZeroPage),
    OpInfo::new(0xb5, LDA, 2, 4, ZeroPageX),
    OpInfo::new(0xad, LDA, 3, 4, Absolute),
    OpInfo::new(0xbd, LDA, 3, 4/*+ add 1 cycle if page boundary crossed*/, AbsoluteX),
    OpInfo::new(0xb9, LDA, 3, 4/*+ add 1 cycle if page boundary crossed*/, AbsoluteY),
    OpInfo::new(0xa1, LDA, 2, 6, IndirectX),
    OpInfo::new(0xb1, LDA, 2, 5/*+ add 1 cycle if page boundary crossed*/, IndirectY),
  ];
  pub static ref OPCODES_MAP: [&'static OpInfo; 0x100] = {
    let mut map: [&'static OpInfo; 0x100] = [&BAD_OPCODE; 0x100];
    for op in &*OPCODES {
      map[op.id as usize] = op;
    }
    map
  };
}
