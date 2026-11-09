use v6;
use Test;

# #17206: the type object of `pun but Role` / `Class but Role` and the type
# of its instances are one type: same `.WHICH`, and `===` holds.
plan 9;

role Marker { }
role Box[::T = Any] { has $.v; method make(\v) { self.new(:v(v)) } }

my \T = Box.^pun but Marker;
is T.WHICH, T.make(1).WHAT.WHICH, 'pun but Role: type object and instance type share .WHICH';
ok T.make(1).WHAT === T, 'pun but Role: instance .WHAT === the type object';
ok T === T.make(1).WHAT, 'pun but Role: === is symmetric';

my \U = Box.^pun;
is U.WHICH, U.new.WHAT.WHICH, 'plain pun: type object and instance type share .WHICH';
ok U === U.new.WHAT, 'plain pun: instance .WHAT === the pun';
nok T === U, 'the mixed pun is a different type from the plain pun';

class C { }
my \V = C but Marker;
is V.WHICH, V.new.WHAT.WHICH, 'Class but Role: type object and instance type share .WHICH';
ok V.WHICH ~~ /'C+{Marker}|U'/, 'Class but Role: the .WHICH names the composed type';
ok V.new.WHAT === V, 'Class but Role: instance .WHAT === the type object';
