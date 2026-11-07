# `$OUTERS::x = ...` from a method whose parameter is also named `x` writes the OUTER `state` variable
# (Jupyter::Kernel::Sandbox hands its Supplier to `Std.print` this way) (#16585).
use Test;
plan 5;

state $a;
class K1 { method m($a) { $OUTERS::a = $a } }
K1.new.m(1);
is $a, 1, 'a state variable written through OUTERS from a method with a same-named parameter';

state $b;
class K2 { method m(:$b) { $OUTERS::b = $b } }
K2.new.m(b => 2);
is $b, 2, 'with a named parameter';

class Reader { method get { $a } }
is Reader.new.get, 1, 'a sibling class reads the state variable';

my $c;
class K3 { method m($c) { $OUTERS::c = $c } }
K3.new.m(3);
is $c, 3, 'a plain my variable (always worked)';

state $d;
sub s($d) { $OUTERS::d = $d }
s(4);
is $d, 4, 'from a sub';
