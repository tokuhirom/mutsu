use v6.e.PREVIEW;
use Test;

# ADR-0067, the E6 producer: an lvalue method call whose INVOCANT is a bare
# `is rw` attribute-accessor read hands the callee the attribute's container,
# so a raw-invocant callee writes through it.
#
#     class C { has $.v is rw }; my $c = C.new(v => 42);
#     $c.v.snitch = 9;                  # raku: $c.v is now 9
#
# Everything here is byte-identical under `raku` and `mutsu`. `.snitch` is given
# an explicit snitcher throughout so the observation lands in a variable instead
# of on stderr, which keeps the comparison over stdout alone. A bare `-> $ { }`
# is used rather than `{}`, which is an empty Hash literal.

plan 25;

# --- E6 itself -------------------------------------------------------------

class E6Basic { has $.v is rw }
{
    my $c = E6Basic.new(v => 42);
    my @seen;
    $c.v.snitch(-> $x { @seen.push($x) }) = 9;
    is $c.v, 9, 'E6: a write through a raw invocant reaches the attribute';
    is-deeply @seen, [42], 'E6: the raw-invocant method still observed the old value';
}

class E6Str { has $.v is rw }
{
    my $c = E6Str.new(v => 'hi');
    $c.v.snitch(-> $ { }) = 'yo';
    is $c.v, 'yo', 'E6: a Str-valued attribute';
}

class E6Unset { has $.v is rw }
{
    # An unset attribute has no value to copy, but still names a location.
    my $c = E6Unset.new;
    $c.v.snitch(-> $ { }) = 9;
    is $c.v, 9, 'E6: an unset attribute';
}

class E6Base { has $.v is rw }
class E6Derived is E6Base { }
{
    my $d = E6Derived.new(v => 42);
    $d.v.snitch(-> $ { }) = 9;
    is $d.v, 9, 'E6: an inherited attribute';
}

role E6Role { has $.v is rw }
class E6Composed does E6Role { }
{
    my $c = E6Composed.new(v => 42);
    $c.v.snitch(-> $ { }) = 9;
    is $c.v, 9, 'E6: a role-composed attribute';
}

class E6Self {
    has $.v is rw;
    method go { self.v.snitch(-> $ { }) = 9 }
}
{
    my $c = E6Self.new(v => 42);
    $c.go;
    is $c.v, 9, 'E6: through `self` inside a method';
}

class E6Two { has $.v is rw }
{
    # The container is per-object: writing one must not touch the other.
    my $c = E6Two.new(v => 1);
    my $d = E6Two.new(v => 2);
    $c.v.snitch(-> $ { }) = 9;
    is $c.v, 9, 'E6: the written object changed';
    is $d.v, 2, 'E6: a sibling object did not';
}

class E6Typed { has Int $.v is rw }
{
    # The typed attribute's constraint travels with the container.
    my $c = E6Typed.new(v => 42);
    $c.v.snitch(-> $ { }) = 9;
    is $c.v, 9, 'E6: a typed attribute writes through';
    dies-ok { $c.v = 'x' }, 'E6: and still type-checks afterwards';
}

class E6Inner { has $.w is rw }
class E6Outer { has $.i is rw }
{
    # Depth 2: `$o.i` is an ordinary read, `.w` is the marked invocant.
    my $o = E6Outer.new(i => E6Inner.new(w => 1));
    $o.i.w.snitch(-> $ { }) = 9;
    is $o.i.w, 9, 'E6: a depth-2 accessor chain';
}

class E6Dynamic { has $.v is rw }
{
    # The method name is only known at run time, so the marker carries no name
    # and its gate must let the dispatch through.
    my $c = E6Dynamic.new(v => 42);
    my $m = 'snitch';
    $c.v."$m"() = 9;
    is $c.v, 9, 'E6: a runtime method name';
}

# --- shapes that must keep working exactly as they did ---------------------

class NPlain { has $.v is rw }
{
    my $c = NPlain.new(v => 42);
    $c.v = 9;
    is $c.v, 9, 'plain rw attribute assignment is untouched';
}

class NBind { has $.v is rw }
{
    my $c = NBind.new(v => 42);
    my $x := $c.v;
    $x = 9;
    is $c.v, 9, 'the `:=` bind producer is untouched';
}

class NCopy { has $.v is rw }
{
    # An rvalue read still copies -- the container must not escape into `$r`.
    my $c = NCopy.new(v => 42);
    my $r = $c.v;
    $r = 100;
    is $c.v, 42, 'an rvalue accessor read is still a copy';
}

class NNestedInner { has $.w is rw }
class NNestedOuter { has $.i is rw }
{
    my $o = NNestedOuter.new(i => NNestedInner.new(w => 1));
    $o.i.w = 9;
    is $o.i.w, 9, 'a nested non-raw attribute store still writes the attribute';
    is $o.i.raku, 'NNestedInner.new(w => 9)', 'and the object still renders as itself';
}

class NArray { has $.a is rw }
{
    my $c = NArray.new(a => [1, 2]);
    $c.a[0] = 9;
    is-deeply $c.a, [9, 2], 'an array-valued attribute element store is untouched';
}

class NHash { has $.h is rw }
{
    my $c = NHash.new(h => {a => 1});
    $c.h<a> = 9;
    is $c.h<a>, 9, 'a hash-valued attribute element store is untouched';
}

class NArgAccessor {
    has %.d is rw;
    method get($k) is rw { %!d{$k} }
}
{
    my $c = NArgAccessor.new(d => {foo => 1});
    $c.get('foo').snitch(-> $ { }) = 9;
    is $c.d<foo>, 9, 'an argument-carrying rw accessor invocant is untouched';
}

# --- shapes that must keep REFUSING ---------------------------------------

class RNonRw { has $.v }
{
    # Not `is rw`: raku refuses (`Cannot modify an immutable Int`). The marker
    # is emitted, but a non-rw accessor hands back no container, so the
    # assignment stays a refusal rather than becoming silently wrong.
    my $c = RNonRw.new(v => 42);
    dies-ok { $c.v.snitch(-> $ { }) = 9 }, 'a non-rw attribute accessor still refuses';
    is $c.v, 42, 'and the attribute is unchanged';
}

class RSelf { has $.v is rw }
{
    # `.self` is deliberately outside the raw-invocant family (ADR-0067
    # non-goals): raku refuses `$a.self = 5`, and so must the E6 spelling even
    # though the invocant now arrives as a container.
    my $c = RSelf.new(v => 42);
    dies-ok { $c.v.self = 9 }, '`.self` over a container invocant still refuses';
}

class RComputed { method thing { 42 } }
{
    # No accessor at all behind the invocant: a computed value is not a
    # location, and must stay a loud refusal.
    my $c = RComputed.new;
    dies-ok { $c.thing.snitch(-> $ { }) = 9 }, 'a computed invocant still refuses';
}

done-testing;
