use Test;

# A role's attribute keeps its declared type constraint: the role has no class
# of its own to hold `attribute_types`, so the constraint is recorded per
# (role, attribute) and copied onto every consuming class (and onto the punned
# class). Before this it was dropped at role registration, so `role R { has Int
# $.x }` accepted a Str and `.of` on a typed container attribute answered `Mu`.

plan 14;

role R {
    has Int $.x;
    has Int @.a;
    has Str $.s = 'hi';
}
class C does R { }

is C.new(:x(5)).x, 5, 'typed role attribute accepts a matching value';
dies-ok { C.new(:x('no')) }, 'typed role attribute rejects a mistyped value';
dies-ok { C.new.a.push('str') }, 'typed role array attribute rejects a mistyped element';
is C.new.a.of.^name, 'Int', 'typed role array attribute reports its element type';
is C.new.s, 'hi', 'role attribute default still applies';

# The constraint reaches a punned role too (`R.new` puns R to a class).
dies-ok { R.new(:x('no')) }, 'punned role enforces its attribute type';

# A role type parameter in an attribute type resolves per composition.
role P[::T] { has T $.v }
class PI does P[Int] { }
is PI.new(:v(7)).v, 7, 'role type parameter in an attribute type accepts a match';
dies-ok { PI.new(:v('no')) }, 'role type parameter in an attribute type rejects a mismatch';

# An object hash declared as an attribute keys by `.WHICH`, not by stringifying
# the key — `%!c{Str}` used to store under the empty string (and warn about
# "uninitialized value of type Str in string context").
class OH {
    has Callable %!c{Mu:U};
    method poke($t, &v) { %!c{$t} = &v }
    method peek($t) { %!c{$t} }
    method n() { %!c.elems }
}
my $oh = OH.new;
$oh.poke(Str, sub ($x) { 'from-Str' });
$oh.poke(Int, sub ($x) { 'from-Int' });
is $oh.n, 2, 'two distinct type-object keys in an attribute object hash';
is $oh.peek(Str).('x'), 'from-Str', 'Str key reads back its own value';
is $oh.peek(Int).('x'), 'from-Int', 'Int key reads back its own value';

# The same declared on a role, then composed.
role OHR { has %!c{Mu:U}; method poke($t, $v) { %!c{$t} = $v }; method peek($t) { %!c{$t} } }
class OHC does OHR { }
my $ohc = OHC.new;
$ohc.poke(Str, 'sv');
$ohc.poke(Int, 'iv');
is $ohc.peek(Str), 'sv', 'object-hash attribute from a role keeps the Str key';
is $ohc.peek(Int), 'iv', 'object-hash attribute from a role keeps the Int key';

# A public object-hash attribute is reachable through its accessor.
class OHP {
    has %.c{Mu:U};
    method poke($t, $v) { %!c{$t} = $v }
}
my $ohp = OHP.new;
$ohp.poke(Str, 'via-accessor');
is $ohp.c{Str}, 'via-accessor', 'public object-hash attribute reads through its accessor';
