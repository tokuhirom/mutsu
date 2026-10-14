use Test;

# Two lowering details of the round trip: a unit that declares `class Nil`
# means that class by `Nil`, not the setting's Nil term; and a parenthesised
# declaration passed as a call argument keeps its default.

plan 3;

is Q|class Nil { method greet { 'hi' } }; Nil.greet|.AST.EVAL, 'hi',
    'a declared class Nil is not the Nil term';
is Q|sub f($x) { $x }; f((my Int $d is default(42)))|.AST.EVAL, 42,
    'a parenthesised declaration argument yields its default';
is Q|my $r = (my Int $e is default(7)); $r|.AST.EVAL, 7,
    'a parenthesised declaration assigned on';
