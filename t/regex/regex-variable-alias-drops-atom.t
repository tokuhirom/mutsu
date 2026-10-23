use v6.d;
use Test;

plan 8;

# `$foo=ATOM` in a regex interpolates `$foo` and drops the atom, as Rakudo's
# grammar does; the `=` is not matched literally (#14628).
my $foo = "bc";
my regex two { .. }

is ("abcd" ~~ m/a $foo=[<two>] d/).Str, 'abcd', 'the aliased atom is dropped';
nok "abc=bcd" ~~ m/a $foo=[<two>] d/, 'the `=` is not a literal';
is $foo, 'bc', 'the variable is not assigned';
is ("abcd" ~~ m/a $foo = [<two>] d/).Str, 'abcd', 'whitespace around `=` is the same alias';
nok "abxd" ~~ m/a $foo='x' d/, 'a quoted literal atom is dropped too';
is ("abcd" ~~ m/a $foo=<two>+ d/).Str, 'abcd', 'a quantified assertion atom';
is ("abcd" ~~ m/a $foo d/).Str, 'abcd', 'a plain interpolation still works';

my @x = <a b>;
is ("abx" ~~ m/@x=. /).Str, 'a', 'an array variable aliases the same way';
