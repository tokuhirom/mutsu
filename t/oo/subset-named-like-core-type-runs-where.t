use Test;
use nqp;

# A subset may reuse a core type's name (`subset Int of Any where ...`); it
# shadows the core type, so every type check against the name runs its
# `where` -- including for a value the core check alone would accept (#19013).

plan 8;

subset Int of Any where { $_ ~~ Str };

is nqp::istype(5, Int), 0, 'nqp::istype runs the where for a native Int';
is nqp::istype("x", Int), 1, 'nqp::istype accepts what the where accepts';
nok 5 ~~ Int, '~~ runs the where for a native Int';
ok "x" ~~ Int, '~~ accepts what the where accepts';

my Int $v;
throws-like { $v = 5 }, X::TypeCheck::Assignment, 'a variable typed with the subset checks it';
lives-ok { $v = "y" }, '... and takes what it accepts';

my $count = 0;
subset Str of Any where { $count++; True };
ok "s" ~~ Str, 'a Str-named subset accepts';
is $count, 1, 'and its where ran once';
