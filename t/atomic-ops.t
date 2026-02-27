use Test;

plan 4;

my Int $x = 1;
is ⚛$x, 1, 'atomic fetch on scalar variable';
is ($x ⚛= 2), 2, 'atomic assignment returns assigned value';
is $x, 2, 'atomic assignment updates the variable';

my atomicint $i = 0;
await start { for ^1000 { $i⚛++; } } xx 4;
is atomic-fetch($i), 4000, 'atomic postfix increment works across start/await';
