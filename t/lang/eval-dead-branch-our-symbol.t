use Test;
use experimental :rakuast;

plan 8;

# rakudo installs an `our` declaration's package symbol when the unit is
# COMPILED, so an EVAL'd unit has it even in a branch that never runs.

is EVAL(Q|if False { our $o = 4 }; OUR::<$o>.^name|), 'Any', 'string EVAL: a dead-branch our scalar is installed, undefined';
is EVAL(Q|if False { our @a; our %h }; OUR::<@a>.elems|), 0, 'string EVAL: a dead-branch our array is an empty container';
is EVAL(Q|if False { our @a2; our %h2 }; OUR::<%h2>.elems|), 0, 'string EVAL: a dead-branch our hash is an empty container';
is EVAL(Q|if True { our $run = 9 }; OUR::<$run>|), 9, 'string EVAL: a reached our keeps its value';

is EVAL(Q|if False { our @t; our %u }; OUR::<@t>.elems|.AST), 0, 'tree EVAL: a dead-branch our array is empty';
is EVAL(Q|if False { our $s }; OUR::<$s>.^name|.AST), 'Any', 'tree EVAL: a dead-branch our scalar is undefined';
is EVAL(Q|class Foo { if False { our $c = 1 } }; Foo::<$c>.^name|), 'Any', 'a dead-branch our in a class lands in that class';
is EVAL(Q|module M { if False { our $m = 1 } }; M::<$m>.^name|), 'Any', 'a dead-branch our in a module lands in that module';
