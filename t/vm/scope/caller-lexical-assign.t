use v6;
use Test;

# Source: DirHandle (`CALLER::LEXICAL::<$_> = ...`). Assigning through
# `CALLER::LEXICAL::<$name>` reaches any lexical of the caller, not only a
# dynamic one, from a sub as well as from a method.

plan 3;

sub setit() { CALLER::LEXICAL::<$x> = 5 }
my $x = 1;
setit();
is $x, 5, 'sub assigns a caller lexical';

class D {
    method m() { CALLER::LEXICAL::<$y> = 6 }
    multi method n(Mu:U) { CALLER::LEXICAL::<$y> = 7 }
}
my $y = 1;
D.new.m;
is $y, 6, 'method assigns a caller lexical';
D.new.n(Mu);
is $y, 7, 'multi method with a type-object invocant argument assigns it too';
