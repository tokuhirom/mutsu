use Test;

plan 8;

ok !defined(0.lsb), '0.lsb is Nil';
is 1.lsb, 0, '1.lsb is 0';
is 256.lsb, 8, '256.lsb is 8';
is (-128).lsb, 7, '(-128).lsb is 7';

ok !defined(0.msb), '0.msb is Nil';
is 256.msb, 8, '256.msb is 8';
is (-127).msb, 7, '(-127).msb is 7';
is (-129).msb, 8, '(-129).msb is 8';
