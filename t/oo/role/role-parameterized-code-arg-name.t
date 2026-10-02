use Test;

plan 5;

my role Q[&f] {}
sub foo($a) { }

is Q[-> $, $ { 1 }].^name, 'Q[Block]', 'pointy block argument is named Block';
is Q[{ 1 }].^name, 'Q[Block]', 'bare block argument is named Block';
is Q[&foo].^name, 'Q[Sub]', 'sub argument is named Sub';
is Q[*+1].^name, 'Q[WhateverCode]', 'WhateverCode argument is named WhateverCode';

my role R[$x] {}
is R[-> { 1 }].^name, 'R[Block]', 'untyped role parameter, pointy block argument';
