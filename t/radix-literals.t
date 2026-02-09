use Test;
plan 6;

is 0xFF, 255, '0xFF is 255';
is 0x10, 16, '0x10 is 16';
is 0o77, 63, '0o77 is 63';
is 0o10, 8, '0o10 is 8';
is 0b1010, 10, '0b1010 is 10';
is 0b11111111, 255, '0b11111111 is 255';
