# A `does`/`but` role mixin at the point of declaration (`my @a does R1`,
# `my $x but Role`) applies the role to the freshly-declared variable, so its
# methods are callable. Previously this parsed as a stray statement-level
# `DoesDecl` (a no-op outside a class body), leaving the variable un-mixed.
# NOTE: this covers the runtime container-mixin cases (Array/Hash and the
# `but` value form). Mixing into an *undefined Scalar* container
# (`my $x does R; say $x.WHAT` => Scalar+{R}) needs a compile-time container
# trait and is not yet supported.
use Test;
plan 4;

role R1 { method test { 42 } }
role Named { method name { 'mixed' } }

my @array does R1;
is @array.test, 42, 'my @array does R1 — role method callable on the array';

my %h does R1;
is %h.test, 42, 'my %h does R1 — role method callable on the hash';

{
    my @inner does R1;
    is @inner.test, 42, 'decl-time does works inside a nested block';
}

# a plain declaration with no does/but is unaffected
my @plain;
is @plain.elems, 0, 'a plain declaration is unaffected';
