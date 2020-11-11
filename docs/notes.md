64kb of memory at most addressable

Processor is little endian - least significant bytes first

first 256 bytes of memory ($0000-$00FF) is "Zero Page"

second page ($0100-$01FF) is reserved for system stack and cannot be
relocated

The only other reserved locations in the memory map are the very last 6 bytes of memory $FFFA to $FFFF which must be programmed with the addresses of the non-maskable interrupt handler ($FFFA/B), the power on reset location ($FFFC/D) and the BRK/interrupt request handler (\$FFFE/F) respectively.

## 10/22/2020

trying to figure out if one function per op code is the right approach, or if
there should be some clever trick with the address modes

## 11/10/2020

The NES actually used a Ricoh 2A03 processor, not a 6502. Although they
have identical opcodes, the only difference is that the NES doesn't
support the BCD (binary coded decimal) setting. Not sure if it should
still support setting the flag though - the status can be pushed to the
stack so it is possible that some games use it as an extra status bit
for some reason?

## 11/11/2020

Finally figured out that pc is initialized to the 16 bit address found at the
reset vector, thanks to http://forums.nesdev.com/viewtopic.php?t=3677
