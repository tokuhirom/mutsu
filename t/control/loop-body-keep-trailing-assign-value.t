use Test;

# A loop body's KEEP/UNDO is decided by the DEFINEDNESS of the body's value, and
# a trailing assignment evaluates to the assigned variable. annotations 0.1.0
# (`Buffer.STAMP`) hit this: its KEEP `take` was lost. #15295

plan 7;

my $count = 0;
is-deeply (gather for 6..8 -> $v { KEEP { take $v }; $count += 1 }).List,
    (6, 7, 8), 'KEEP runs when the body ends in `$n += 1`';

my $c2 = 0;
is-deeply (gather for 6..8 -> $v { my $b = $v; KEEP { take $b }; $c2++ }).List,
    (6, 7, 8), 'KEEP runs when the body ends in `$n++` (first value 0 is defined)';

my $x;
is-deeply (gather for 1..2 -> $v { KEEP { take "k$v" }; $x = 0 }).List,
    ("k1", "k2"), 'a falsy-but-defined trailing assignment still runs KEEP';

is-deeply (gather for 1..2 -> $v { KEEP { take "k$v" }; UNDO { take "u$v" }; my $e = ""; $e }).List,
    ("k1", "k2"), 'an empty-string body value runs KEEP';

is-deeply (gather for 1..2 -> $v { KEEP { take "k$v" }; UNDO { take "u$v" }; Nil }).List,
    ("u1", "u2"), 'a Nil body value runs UNDO';

my @seen;
for 1..2 { KEEP { @seen.push("keep$_") }; UNDO { @seen.push("undo$_") }; my $y = 0; $y += 5 }
is-deeply @seen, ["keep1", "keep2"], 'plain for loop: trailing `+=` runs KEEP';

my $z = Any;
my @s2;
for 1..2 { KEEP { @s2.push("keep") }; UNDO { @s2.push("undo$_") }; $z = Any }
is-deeply @s2, ["undo1", "undo2"], 'a trailing assignment of an undefined value runs UNDO';
