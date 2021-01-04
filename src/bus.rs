use crate::bit::{make_u16, split_u16};

pub trait Bus {
    fn read(&self, addr: u16) -> u8;

    fn read_u16(&self, addr: u16) -> u16 {
        let lo = self.read(addr);
        let hi = self.read(addr + 1);
        make_u16(lo, hi)
    }

    fn write(&mut self, addr: u16, val: u8);

    fn write_u16(&self, addr: u16, val: u16) {
        let (lo, hi) = split_u16(val);
        self.write(addr, lo);
        self.write(addr + 1, hi);
    }
}
