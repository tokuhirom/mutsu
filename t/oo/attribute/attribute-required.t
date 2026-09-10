use v6;
use Test;

plan 10;

# `.required` on an Attribute introspection object reports the `is required`
# trait state: Mu (not required), 1 (bare `is required`), or the reason string.

# Bare `is required` -> 1 (an Int)
class Bare { has $.x is required }
my $bare = Bare.^attributes[0].required;
is $bare, 1, 'bare is required returns 1';
ok $bare.Bool, 'bare is required is truthy';
isa-ok $bare, Int, 'bare is required is an Int';

# Not required -> the type object Mu
class Plain { has $.x }
my $plain = Plain.^attributes[0].required;
nok $plain.defined, 'plain attribute .required is undefined (Mu)';
nok $plain.Bool, 'plain attribute .required is falsy';

# `is required("reason")` -> the reason string
class WithReason { has $.x is required("need an x") }
is WithReason.^attributes[0].required, 'need an x',
    'is required(reason) returns the reason string';

# A typed required attribute still reports required-ness
class Typed { has Int $.n is required }
is Typed.^attributes[0].required, 1, 'typed is required returns 1';

# Required carried through a role
role R { has $.r is required }
class Consumer does R {}
is Consumer.^attributes[0].required, 1, 'role attribute is required survives composition';

# Multiple attributes each report independently
class Mixed { has $.a is required; has $.b }
my @req = Mixed.^attributes.map({ .required.defined });
is @req[0], True, 'first attribute is required';
is @req[1], False, 'second attribute is not required';
