use v6;
use Test;

plan 8;

# `&CORE::infix:<op>` names the core operator routine (not the string "op").
class A { }
my $x = A.new;
is &CORE::infix:<eqv>($x, $x), True, '&CORE::infix:<eqv> is callable';
is &CORE::infix:<+>(1, 2), 3, '&CORE::infix:<+>';
is &CORE::infix:<~>("a", "b"), "ab", '&CORE::infix:<~>';

my &mul = &CORE::infix:<*>;
is mul(3, 4), 12, 'bound to a lexical routine variable';
is &CORE::prefix:<->(5), -5, '&CORE::prefix:<->';
is &CORE::postfix:<++>.^name, 'Sub', '&CORE::postfix:<++> is a routine';
is-deeply (1, 2, 3).map(&CORE::infix:<*>.assuming(2)).List, (2, 4, 6), 'usable as an argument';
is CORE::<&infix:<+>>(1, 2), 3, 'the CORE:: subscript form still works';
