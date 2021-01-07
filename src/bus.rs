use crate::bit::{make_u16, split_u16};

pub trait Bus {
    fn read(&self, addr: u16) -> u8;

    fn read_u16(&self, addr: u16) -> u16 {
        let lo = self.read(addr);
        let hi = self.read(addr + 1);
        make_u16(lo, hi)
    }

    fn write(&mut self, addr: u16, val: u8);

    fn write_u16(&mut self, addr: u16, val: u16) {
        let (lo, hi) = split_u16(val);
        self.write(addr, lo);
        self.write(addr + 1, hi);
    }
}

impl Bus for Vec<u8> {
    fn read(&self, addr: u16) -> u8 {
        self[addr as usize]
    }

    fn write(&mut self, addr: u16, val: u8) {
        self[addr as usize] = val;
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_read_u16() {
        let bus = vec![0xCD, 0xAB];
        assert_eq!(0xABCD, bus.read_u16(0));
    }

    #[test]
    fn test_write_u16() {
        let mut bus = vec![0; 2];
        bus.write_u16(0, 0xABCD);

        assert_eq!(bus[0], 0xCD);
        assert_eq!(bus[1], 0xAB);
    }
}
