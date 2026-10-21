use Test;

# A `Proxy` subclass with attributes and methods: its methods read and write
# `$!attr` (plain, `:=`, `⚛`, `cas`) on the instance's own attribute store,
# its private methods are callable on the instance's `.VAR`, and an attribute
# without a default starts as its type object or `is default` value, as an
# ordinary instance's does. AttrX::Mooish's `AttrProxy` is built this way
# (#14259). Expected values are the `raku` (v2026.09) output.

plan 10;

class AP is Proxy {
    has Bool $!is-set is default(False);
    has $!attr;
    has Bool $!flag is default(False);
    has $!promise;
    method !SET-SELF(%a) is raw { $!attr := %a<attribute>; self }
    method new(:$FETCH, :$STORE, *%a) is raw {
        callwith(:$FETCH, :$STORE).VAR!SET-SELF(%a)
    }
    method info { "set=$!is-set attr=$!attr" }
    method mark { $!is-set = True }
    method flag { ⚛$!flag }
    method raise { $!flag ⚛= True }
    method acquire {
        my $old = ⚛$!promise;
        !$old.defined && cas($!promise, $old, Promise.new) === $old
    }
    method promise-status { (⚛$!promise).status }
}

my $p := AP.new(:attribute<X>, FETCH => -> $ { 1 }, STORE => -> $, $ { });
is $p, 1, 'the instance is still a container that FETCHes';
is $p.VAR.info, 'set=False attr=X', 'a private method called from new bound an attribute';
$p.VAR.mark;
is $p.VAR.info, 'set=True attr=X', 'a method writes an attribute';
is $p.VAR.flag, False, 'an attribute without an initializer starts at its is default value';
$p.VAR.raise;
is $p.VAR.flag, True, 'an atomic store to an attribute lands';
ok $p.VAR.acquire, 'cas on an attribute swaps in the new value';
is $p.VAR.promise-status, 'Planned', '... which an atomic fetch then reads';
nok $p.VAR.acquire, 'a second cas sees the stored value';

my $q := AP.new(:attribute<Y>, FETCH => -> $ { 2 }, STORE => -> $, $ { });
is $q.VAR.info, 'set=False attr=Y', 'each instance has its own attributes';
is $p.VAR.info, 'set=True attr=X', '... and the first instance kept its own';
