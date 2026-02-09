use Test;
plan 7;

is 0xFF +& 0x0F, 15, 'bitwise AND';
is 0x0F +| 0xF0, 255, 'bitwise OR';
is 0xFF +^ 0x0F, 240, 'bitwise XOR';
is 1 +< 8, 256, 'left shift';
is 256 +> 4, 16, 'right shift';
is 0b1010 +& 0b1100, 0b1000, 'binary AND';
is 0b1010 +| 0b0101, 0b1111, 'binary OR';
