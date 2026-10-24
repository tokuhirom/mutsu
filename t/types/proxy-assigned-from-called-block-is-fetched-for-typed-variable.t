use v6;
use Test;

plan 11;

# `=` reads its right-hand side in value context: a Proxy assigned into a
# variable is FETCHed before the type check and the store. That holds when the
# assignment runs inside a block that is called (the write has no local slot
# for the captured variable), exactly as it does for a statement.
class Foo { has $.x }
my $p := Proxy.new(FETCH => -> $ { Foo.new(:x(3)) }, STORE => -> $, $v { });
sub run(&f) { f() }

my Foo $a;
$a = $p;
is $a.^name, "Foo", "a statement assignment";

my Foo $b;
{ $b = $p }
is $b.^name, "Foo", "a bare block";

my Foo $c;
run({ $c = $p });
is $c.^name, "Foo", "a block passed to a sub";

my Foo $d;
my &blk = { $d = $p };
blk();
is $d.^name, "Foo", "a closure stored in a variable";

sub ret-proxy is rw { $p }
my Foo $e;
run({ $e = ret-proxy() });
is $e.^name, "Foo", "a Proxy returned by an is-rw sub";

# The Proxy stays a container where it is bound or stored through.
my $store = 0;
my $backing = 10;
my $q := Proxy.new(FETCH => -> $ { $backing }, STORE => -> $, $v { $store++; $backing = $v });
run({ $q = 5 });
is $store, 1, "assigning TO a bound Proxy from a block still calls STORE";
is $backing, 5, "and STORE received the value";

my $copy = 7;
run({ $copy = $q });
is $copy, 5, "assigning a Proxy FROM a block copies its FETCHed value";
$backing = 99;
is $copy, 5, "and the copy no longer follows the Proxy";

my $alias := $q;
run({ $alias = 3 });
is $backing, 3, "a := alias of the Proxy takes STORE from a block";

my Int $typed;
run({ $typed = $q });
is $typed, 3, "a typed Int variable gets the FETCHed value";
