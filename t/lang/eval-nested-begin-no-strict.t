use Test;
use experimental :rakuast;

plan 8;

# A BEGIN nested in a routine or block of an EVAL'd unit, written under
# `no strict`, runs at the unit's BEGIN time whether it reads an auto-declared
# package variable or a lexical of the EVAL's caller.

my @log;
EVAL Q[sub in-routine { no strict; BEGIN { $auto1 = 3; @log.push("routine $auto1") } }];
is @log.join('|'), 'routine 3', 'string EVAL: a BEGIN under no strict in a routine, last statement';

@log = ();
EVAL Q[{ no strict; BEGIN { $auto2 = 4; @log.push("block $auto2") } }];
is @log.join('|'), 'block 4', 'string EVAL: a BEGIN under no strict in a block';

@log = ();
EVAL Q[sub arr { no strict; BEGIN { @auto3 = 1, 2; @log.push("array @auto3[]") } }];
is @log.join('|'), 'array 1 2', 'string EVAL: an auto-declared array';

@log = ();
EVAL Q[sub in-routine2 { no strict; BEGIN { $auto4 = 5; @log.push("tree $auto4") } }].AST;
is @log.join('|'), 'tree 5', 'tree EVAL: a BEGIN under no strict in a routine';

my $v = 5;
@log = ();
EVAL Q[sub reads-caller { no strict; BEGIN { @log.push("v=$v") } }];
is @log.join('|'), 'v=5', 'a lexical of the caller is read by the BEGIN';

EVAL Q[sub writes-caller { no strict; BEGIN { $v = 7 } }];
is $v, 7, 'and written by it';

@log = ();
EVAL Q[sub two-a { no strict; BEGIN { $dup = 1; @log.push("first $dup") } }; sub two-b { no strict; BEGIN { $dup = 2; @log.push("second $dup") } }];
is @log.join('|'), 'first 1|second 2', 'two BEGINs may auto-declare one name between them';

@log = ();
EVAL Q[sub never-called { no strict; BEGIN { $auto5 = 8; @log.push("ran $auto5") } }];
is @log.elems, 1, 'the BEGIN ran though its routine never did';
