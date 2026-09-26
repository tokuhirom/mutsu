use Test;
use nqp;

# `nqp::getattr` / `nqp::bindattr` / `nqp::create` read and write one
# attribute in place, and `nqp::create` starts from a per-class slot template
# (ADR-0121 D1). These pin what that must not change.

plan 23;

class P {
    has int $!n;
    has str $!s;
    has Int $!typed;
    has $!plain;
    has @!items;
}

# create: a native int attribute reads as 0 and an unset object attribute
# is undefined.
{
    my $p := nqp::create(P);
    is nqp::getattr_i($p, P, '$!n'), 0, 'a created native int attribute reads 0';
    ok !nqp::getattr($p, P, '$!typed').defined, 'a created typed attribute is undefined';
    ok !nqp::getattr($p, P, '$!plain').defined, 'a created untyped attribute is undefined';
}

# Two creates are independent objects: the cached template is copied.
{
    my $a := nqp::create(P);
    my $b := nqp::create(P);
    nqp::bindattr($a, P, '$!plain', 'A');
    is nqp::getattr($a, P, '$!plain'), 'A', 'bindattr writes the first object';
    ok !nqp::getattr($b, P, '$!plain').defined, 'the second object is untouched';
    my $c := nqp::create(P);
    ok !nqp::getattr($c, P, '$!plain').defined, 'a later create is not seeded by an earlier bind';
}

# A container attribute is found under its bare name.
{
    my $p := nqp::create(P);
    nqp::bindattr($p, P, '@!items', [1, 2, 3]);
    is nqp::getattr($p, P, '@!items').elems, 3, 'a sigil-twigil operand finds the bare-keyed attribute';
}

# bindattr inside a method while its own attribute is being read.
class R {
    has $!x = 1;
    method bump { nqp::bindattr(self, R, '$!x', $!x + 1); $!x }
}
is R.new.bump, 2, 'bindattr from a method on self';

# p6bindattrinvres returns the invocant, with the attribute bound.
{
    my $p := nqp::p6bindattrinvres(nqp::create(P), P, '$!plain', 'x');
    is nqp::getattr($p, P, '$!plain'), 'x', 'p6bindattrinvres binds the attribute';
}

# Map / List storage reads hand back the container itself.
{
    my %h = a => 1;
    my $st := nqp::getattr(%h, Map, '$!storage');
    nqp::bindkey($st, 'b', 2);
    is %h<b>, 2, "a Map's \$!storage is the hash itself";
    my @l = 1, 2;
    is nqp::getattr(@l, List, '$!reified').elems, 2, "a List's \$!reified is the array itself";
}

# `nqp::create` is also used to allocate a Map subclass before
# `p6bindattrinvres` installs the JSON parser's hash storage. This is the
# shape used by RepositoryEvent's `bless-hash-as` helper.
class NqpMapSubclass is Map { }
{
    my %source = a => 1;
    my $map := nqp::p6bindattrinvres(
        nqp::create(NqpMapSubclass), NqpMapSubclass, '$!storage',
        nqp::getattr(%source, Map, '$!storage')
    );
    is $map<a>, 1, 'p6bindattrinvres installs storage into a Map subclass';
    is nqp::getattr($map, Map, '$!storage')<a>, 1,
        'getattr exposes a Map subclass backing store';
}

# Pair's core slots are reachable through the same NQP attribute operations as
# user-defined instance slots. The Pair payload is shared, so both writes must
# be visible through the ordinary Pair API as well.
{
    my $p := nqp::decont('a' => 1);
    is nqp::getattr($p, Pair, '$!key'), 'a', "Pair's \$!key is readable";
    is nqp::getattr($p, Pair, '$!value'), 1, "Pair's \$!value is readable";
    nqp::bindattr($p, Pair, '$!key', 'b');
    is nqp::getattr($p, Pair, '$!key'), 'b', "Pair's \$!key is writable";
    nqp::bindattr($p, Pair, '$!value', 2);
    is nqp::getattr($p, Pair, '$!value'), 2, "Pair's \$!value is writable";
    is $p.key, 'b', 'a bound Pair key is visible through Pair.key';
    is $p.value, 2, 'a bound Pair value is visible through Pair.value';
    is $p.raku, ':b(2)', 'a bound Pair renders with its updated slots';
    my $copy := $p;
    is nqp::getattr($copy, Pair, '$!key'), 'b', 'a copied Pair shares its key slot';
}

# VMHash / VMArray storage classes, matched by short name.
my class Outer::Store is repr('VMHash') { }
my class Outer::Buf is repr('VMArray') { }
{
    my $h := nqp::create(Outer::Store);
    nqp::bindkey($h, 'k', 'v');
    is nqp::atkey($h, 'k'), 'v', 'a qualified VMHash class creates a hash store';
    my $a := nqp::create(Outer::Buf);
    nqp::push($a, 7);
    is nqp::atpos($a, 0), 7, 'a qualified VMArray class creates an array store';
}
