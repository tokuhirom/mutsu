use Test;

# A Proxy used as a subscript or a pair key is read by value: an element
# store or increment addresses the element its FETCH answers, and a FETCH
# that answers another container is followed down to the value (the
# annotations module's `Binder`, a Proxy subclass holding a Proxy).

plan 14;

my $p := Proxy.new(FETCH => method { 'k' }, STORE => method ($) { });
my %g = k => 5;
is %g{$p}, 5, 'a Proxy key reads its element';
%g{$p}++;
is-deeply %g, {k => 6}, 'a Proxy key increments its element';
%g{$p} = 9;
is-deeply %g, {k => 9}, 'a Proxy key assigns its element';
%g{$p} += 1;
is-deeply %g, {k => 10}, 'a Proxy key compound-assigns its element';

my @a = 1, 2, 3;
my $q := Proxy.new(FETCH => method { 1 }, STORE => method ($) { });
@a[$q] = 9;
is-deeply @a, [1, 9, 3], 'a Proxy index assigns its element';
@a[$q]++;
is-deeply @a, [1, 10, 3], 'a Proxy index increments its element';

my $inner := Proxy.new(FETCH => method { 'n' }, STORE => method ($) { });
my $outer := Proxy.new(FETCH => method { $inner }, STORE => method ($) { });
is ($outer => 1).key, 'n', 'a nested Proxy pair key reads its value';
my %k = ($outer => 1);
is-deeply %k, {n => 1}, 'a hash built from a nested Proxy key';
is Map.new(($outer => 1)).keys, ('n',), 'a Map built from a nested Proxy key';
my %s;
%s{$outer} = 2;
is-deeply %s, {n => 2}, 'a nested Proxy subscript stores under its value';

my class Binder is Proxy {
    has $!value is default('dflt');
    method new is raw { callwith :&FETCH, :&STORE }
    method BIND(Mu \SELF: Mu $topic is raw --> Nil) { STORE SELF, $topic }
    my method FETCH is raw { $!value }
    my method STORE(Mu $topic is raw --> Nil) { $!value := $topic }
}
my $slot := Binder.new;
$slot.VAR.BIND($inner);
is $slot, 'n', 'a Proxy subclass holding a Proxy reads its value';
is Map.new(($slot => 1)).keys, ('n',), 'and keys a Map by it';
my @bound := Array.new;
@bound.BIND-POS(0, $slot);
is Map.new(@bound Z=> (1,)).keys, ('n',), 'also through a bound array slot';
sub raw-keys(@of is raw) { Map.new: reverse @of Z=> (1,) }
is raw-keys(@bound).keys, ('n',), 'and through an is raw parameter';
