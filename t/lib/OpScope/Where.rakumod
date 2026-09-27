# An exported operator candidate whose `where` clause records every call, so a
# test can tell whether some other module's arithmetic consulted it.
unit module OpScope::Where;
our $calls = 0;
multi infix:<*>(Int $n where { $calls++; False }, Int $m) is export { "where" }
