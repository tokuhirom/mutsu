use Test;

plan 8;

# `has %!d` and `has $.d` are distinct attributes that share a bare name
# (#12528): the public accessor and `new`'s named arg address the `$.d`
# slot, and the private `%!d` keeps its own.
class C2 {
    has %!d;
    has $.d;
    method s { %!d<k> = 1; self }
    method pd { %!d.raku }
}
my $c2 = C2.new(:d(5));
is $c2.s.pd, '{:k(1)}', 'private %!d keeps its own slot';
is $c2.d, 5, 'public $.d reads the named arg';

class Dflt { has @!l; has $.l = 3; method n { @!l.elems } }
is Dflt.new.l, 3, 'public $.l default survives a private @!l';
is Dflt.new.n, 0, 'private @!l stays empty';

# Across a parent/child pair.
class P { has %!d; method pd { %!d.raku } method s { %!d<z> = 2; self } }
class C is P { has $.d }
my $c = C.new(:d(5));
is $c.d, 5, 'child $.d binds the named arg';
is $c.s.pd, '{:z(2)}', 'parent %!d is not clobbered by the child $.d';
is C.^attributes.first(*.name eq '%!d').get_value(C.new).raku, '{}',
    'get_value of the parent %!d reads its own slot';
is C.^attributes.first(*.name eq '$!d').get_value($c), 5,
    'get_value of the child $!d reads the public slot';
