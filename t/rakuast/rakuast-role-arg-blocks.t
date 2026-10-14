use v6;
use experimental :rakuast;
use Test;

# Code-valued arguments of a parameterised role (`does R[...]`) and of a regex
# assertion survive the RakuAST round trip: WhateverCode, a bare block, a
# placeholder block and an empty-signature pointy block.

plan 8;

sub class-of(Str $src) { $src.AST.statements[*-1].expression }

ok class-of(Q｢role R1[&f] { }; class A1 does R1[{ 42 }] { }｣).gist.contains('RakuAST::Block.new('),
    'a bare block argument is a Block node';
ok class-of(Q｢role R2[&f] { }; class A2 does R2[* + 1] { }｣).gist.contains('RakuAST::Term::Whatever.new'),
    'a WhateverCode argument is an ApplyInfix over Whatever';

is EVAL(Q｢role R3[&f] { method apply(|c) { f(|c) } }; class A3 does R3[{ 42 }] { }; A3.new.apply｣.AST), 42,
    'bare block binds to &f';
is EVAL(Q｢role R4[&f] { method apply(|c) { f(|c) } }; class A4 does R4[{ $^n * 2 }] { }; A4.new.apply(21)｣.AST), 42,
    'placeholder block binds to &f';
is EVAL(Q｢role R5[&f] { method apply(|c) { f(|c) } }; class A5 does R5[* + 1] { }; A5.new.apply(41)｣.AST), 42,
    'WhateverCode binds to &f';

is EVAL(Q｢proto sub infix:<precedes>($, $) {*}
          role D6[&infix:<precedes>] { method t($a, $b) { $a precedes $b } }
          class M6 does D6[* cmp * == More] { }
          M6.new.t(1, 2)｣.AST), False, 'WhateverCode role argument is the operator';

is EVAL(Q｢my $called = 0;
          grammar G7 { token TOP { <e(-> { $called++; True })> }; token e(&c) { a <?{ c() }> } }
          G7.parse('a'); $called｣.AST), 1, 'a pointy block argument runs once';

is EVAL(Q｢my regex y ($f) { a }; my $r = 0; my $m = 'a' ~~ / <y({ $r++; 1 }())> /; $r｣.AST), 1,
    'a block call argument runs once';
