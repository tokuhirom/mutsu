use Test;

# `my @m = LIST-OF-PROXIES` stores the values, not the Proxy containers, so
# binding an element and assigning to it does not reach the Proxy's STORE
# (annotations' `Keeper`, #15259).

plan 3;

my @stores;
sub mk($k) is raw {
    my $s = 0;
    Proxy.new(FETCH => -> | { $s }, STORE => -> $, $v { @stores.push("$k=$v"); $s = $v });
}

my @m = <a b>.map(&mk);
for @m -> Mu $b is raw { $b = 7 }
is-deeply @stores, [], 'map result: no STORE reached';

my @n = (mk("x"), mk("y"));
for @n -> Mu $b is raw { $b = 7 }
is-deeply @stores, [], 'list literal: no STORE reached';

my $p = mk("z");
$p = 3;
is-deeply @stores, ["z=3"], 'a Proxy variable still stores';
