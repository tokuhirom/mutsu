use Test;

# `is raw` on an ANONYMOUS sub was parsed and then dropped: `Expr::AnonSub` and
# `Expr::AnonSubParams` carried only `is_rw`, so the trait never reached the
# routine's rw-capability oracle and `($f()) = v` on a `Proxy`-returning
# anonymous `sub ... is raw` refused the write that rakudo accepts. The named
# spelling and `is rw` both worked, which is what hid it.

plan 7;

my $store = 0;

sub named-raw() is raw { Proxy.new(FETCH => { $store }, STORE => -> $, $v { $store = $v }) }
sub named-rw()  is rw  { Proxy.new(FETCH => { $store }, STORE => -> $, $v { $store = $v }) }
my $anon-raw = sub () is raw { Proxy.new(FETCH => { $store }, STORE => -> $, $v { $store = $v }) };
my $anon-rw  = sub () is rw  { Proxy.new(FETCH => { $store }, STORE => -> $, $v { $store = $v }) };

$store = 0; (named-raw()) = 7;  is $store, 7, 'a named `is raw` sub hands out its container';
$store = 0; (named-rw())  = 7;  is $store, 7, 'so does a named `is rw` sub';
$store = 0; ($anon-raw()) = 7;  is $store, 7, 'an anonymous `is raw` sub does too';
$store = 0; ($anon-rw())  = 7;  is $store, 7, 'and an anonymous `is rw` sub';

# Sigilless parameters survive into a Proxy STORE closure, which is how rakudo
# spells `Baggy::AT-KEY` and how mutsu's own native `AT-KEY` base candidate is
# written (`runtime/container_element_proxy.rs`).
my $out = "";
my $two = sub (\obj, \key) is raw {
    Proxy.new(
        FETCH => { "f:" ~ obj ~ ":" ~ key },
        STORE => -> $, \value { $out = "s:" ~ obj ~ ":" ~ key ~ ":" ~ value }
    )
};
is $two("OBJ", "KEY"), 'f:OBJ:KEY', 'FETCH sees both sigilless parameters';
($two("OBJ", "KEY")) = "V";
is $out, 's:OBJ:KEY:V', 'and so does STORE';

# A routine with neither trait still refuses: the fix must not make every
# anonymous sub rw-capable.
my $plain = sub () { Proxy.new(FETCH => { $store }, STORE => -> $, $v { $store = $v }) };
dies-ok { ($plain()) = 1 }, 'an anonymous sub with no trait still refuses';

# vim: expandtab shiftwidth=4
