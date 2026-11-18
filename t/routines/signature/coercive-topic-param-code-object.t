use Test;

# A routine whose parameter is an explicit coercive `$_` (`Str:D() $_`) sees the
# coerced value however it is called: by name, through its code object, or as a
# `.map` callback. The call through the code object bound the parameter and
# then put the raw argument back over `$_`. From CSS::Specification::Compiler's
# `sub lit-ws(Str:D() $_) { .&lit'r.&ws }`, mapped over numbers.
# Expected values checked against raku.

plan 6;

sub a(Str:D() $_) { .^name }
sub b(Str:D() $x) { $x.^name }

is a(35), 'Str', 'called by name';
is &a(35), 'Str', 'called through its code object';
my &f = &a;
is f(35), 'Str', 'called through a code variable';
is-deeply (1, 2).map(&a).List, <Str Str>, 'as a .map callback';
is-deeply (1, 2).map(&b).List, <Str Str>, 'a named coercive parameter, for comparison';

sub c(Int() $_) { $_ + 1 }
is-deeply ('3', '4').map(&c).List, (4, 5), 'an Int() topic parameter';
