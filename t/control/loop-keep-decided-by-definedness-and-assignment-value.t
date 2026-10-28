use Test;

# A loop body's KEEP/UNDO follow its value's definedness: a falsy-but-defined
# value (`0`, the old value of `$n++`) and an assignment statement's value
# (`$n += 1`, `my @a = ...`) are successes; `Nil` is not (#15295).

plan 6;

my @log;
my $n = 0;
for 1..2 { KEEP @log.push("k$_"); UNDO @log.push("u$_"); $n++ }
is @log.join(" "), "k1 k2", 'a post-increment (first value 0) keeps';
@log = ();
for 3..4 { KEEP @log.push("k$_"); UNDO @log.push("u$_"); $n += 1 }
is @log.join(" "), "k3 k4", 'a compound assignment keeps';
@log = ();
for 5..6 { KEEP @log.push("k$_"); UNDO @log.push("u$_"); 0 }
is @log.join(" "), "k5 k6", 'a falsy-but-defined value keeps';
@log = ();
for 7..8 { KEEP @log.push("k$_"); UNDO @log.push("u$_"); Nil }
is @log.join(" "), "u7 u8", 'Nil undoes';
@log = ();
for 9..10 { KEEP @log.push("k$_"); UNDO @log.push("u$_"); my @a = 1, 2 }
is @log.join(" "), "k9 k10", 'a declaration with an initializer keeps';

my $c = 0;
is (gather for 6..8 -> $v { KEEP { take $v }; $c += 1 }).list, (6, 7, 8),
    'KEEP { take } after a compound assignment inside a gather';
