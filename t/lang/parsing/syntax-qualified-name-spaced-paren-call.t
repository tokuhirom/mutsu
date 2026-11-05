use v6;
use Test;

# From Pop (lib/Pop/Drawable.rakumod): `SDL::GetTextureBlendMode ($!texture, $mode)
# and die ...` -- a package-qualified routine whose package is not loaded here,
# called with a space before the parenthesized list. mutsu read the capitalised
# last component as a type and died with "Two terms in a row".
# (`Foo::Get (...)` passes ONE argument, the parenthesized list, in raku too.)

plan 5;

my $src = q:to/END/;
    sub f { my $m = 3; Unloaded::Get ($m, $m) and 1; }
    sub g { Unloaded::Get ($_) or 2; }
    1
    END
lives-ok { EVAL $src }, 'spaced paren after an unloaded qualified Capitalised name parses';

# The routine itself decides what it receives: one list argument.
module Pkg { our sub Count(|c) { c.elems } }
is (Pkg::Count (1, 2, 3)), 1, 'a spaced parenthesized list is one argument';
is Pkg::Count(1, 2, 3), 3, 'adjacent parens are the argument list';

# Types stay terms: a set operator after a known type is an infix.
ok (1 (elem) (1, 2)), 'a literal (elem) is still an infix';
my @t = Int, Str;
ok Int (elem) @t, 'Int (elem) @types stays an infix after a known type';
