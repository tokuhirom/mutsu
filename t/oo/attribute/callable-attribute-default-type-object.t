use Test;

# MCP::Client::Leases: `has &.cancelled;` left unset, then `wait-cancelled(&!cancelled)`
# where `sub wait-cancelled(&cancelled)`. An untyped `&` attribute holds the Callable
# type object, which a `&` parameter accepts; mutsu seeded it as `Any` and the bind died
# ("expected Callable but got Any").

plan 10;

my sub is-set(&c --> Bool:D) { return False without &c; True }

class K {
    has &.cancelled;
    has &.other;
    method probe { is-set(&!cancelled) }
    method what  { (&!cancelled).WHAT.raku }
    method other-set { &!other.defined }
}

my $k = K.new;
is $k.what, 'Callable', 'an unset & attribute holds the Callable type object';
is $k.cancelled.WHAT.raku, 'Callable', 'also through the accessor';
nok $k.probe, 'a & parameter accepts it (and reads it as undefined)';
nok $k.other-set, "the second attribute is undefined too";
ok K.new(cancelled => { 1 }).probe, 'a given block is passed on';
nok K.new(cancelled => Callable).probe, 'an explicit Callable type object';

# CREATE seeds the same type object.
is K.CREATE.what, 'Callable', 'CREATE seeds Callable';

# A typed & attribute is unchanged.
class T { has Sub $.s; method w { $!s.WHAT.raku } }
is T.new.w, 'Sub', 'a typed attribute keeps its nominal type';

# Plain scalar attributes still default to Any.
class S { has $.a; method w { $!a.WHAT.raku } }
is S.new.w, 'Any', 'an untyped $ attribute is Any';
class D { has &.f is rw; method d { &!f.defined } }
nok D.new.d, 'an is rw & attribute is undefined';
