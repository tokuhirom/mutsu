# A parameter that aliases the caller's container (`is rw`, `is raw`, `\x`)
# IS that container, so `.VAR.name` names the caller's variable (#11196).
# Found via Test::Describe (`change $a` reports "$a has not changed") and Tee
# (`Tee($file, my $*OUT)` keys on `$one.VAR.?name`).
use Test;

plan 10;

my $a = 42;
sub f($x is rw) { $x.VAR.name }
sub g($x is raw) { $x.VAR.name }
sub h(\x) { x.VAR.name }
sub k(:$v! is raw) { $v.VAR.name }
is f($a), '$a', 'is rw parameter';
is g($a), '$a', 'is raw parameter';
is h($a), '$a', 'sigilless parameter';
is k(:v($a)), '$a', 'named is raw parameter';

sub relay($w is raw) { k(:v($w)) }
is relay($a), '$a', 'an alias of an alias names the original variable';

class T { method m($x is raw) { $x.VAR.?name } }
is T.m($a), '$a', 'method parameter';

sub through-closure($value is raw) { my &f = sub () { $value.VAR.name }; f() }
is through-closure($a), '$a', 'read through a closure over the parameter';

sub dyn($x is raw) { $x.VAR.name }
is dyn(my $*DYN-NAME), '$*DYN-NAME', 'a dynamic variable declared in the argument list';

class C { has $.n; submethod BUILD(:$value! is raw) { $!n = $value.VAR.name } }
my $y = 1;
is C.new(:value($y)).n, '$y', 'through the default constructor into BUILD';

sub own($p) { my $q = 5; $q.VAR.name }
is own($a), '$q', 'a plain lexical still names itself';
