use Test;

# Regression pin: a class nested as `Foo::Any` is a different type from the core
# `Any`, so a `Foo::Any` constraint must not accept everything.
#
# The "qualified name matching" bridge in `Interpreter::type_matches` — which
# lets a type declared under a `unit module` be referred to bare inside it —
# compared only the trailing component. Every class's MRO ends in `Any`, `Mu`,
# so a `Foo::Any` constraint matched that `Any` entry and then took the
# `constraint == "Any"` universal arm: it accepted every instance. Zef hit this
# as `multi method spec-matcher(…::DependencySpecification::Any $spec)`
# swallowing a plain `…::DependencySpecification` and dying on `.specs`, a
# method only the `::Any` sibling has.

plan 9;

class Spec { }
class Spec::Any { has @.specs; }
class Spec::Alt { }

my $plain = Spec.new;
my $any = Spec::Any.new(:specs[1, 2]);

nok $plain ~~ Spec::Any, 'a Spec instance does not smartmatch the nested Spec::Any';
ok $any ~~ Spec::Any, 'a Spec::Any instance does smartmatch Spec::Any';
nok $plain.isa(Spec::Any), 'a Spec instance does not .isa(Spec::Any)';

multi f(Spec::Any $s) { 'ANY' }
multi f($s) { 'GENERIC' }

is f($plain), 'GENERIC', 'multi dispatch prefers the generic candidate for a plain Spec';
is f($any), 'ANY', 'multi dispatch still picks the Spec::Any candidate for a Spec::Any';
is f(42), 'GENERIC', 'a Spec::Any constraint does not swallow an Int';
is f('x'), 'GENERIC', 'a Spec::Any constraint does not swallow a Str';

# The bridge itself must keep working for a genuinely bare-vs-qualified pair,
# and a non-core trailing component is unaffected.
multi g(Spec::Alt $s) { 'ALT' }
multi g($s) { 'GENERIC' }
is g($plain), 'GENERIC', 'a Spec::Alt constraint does not accept a plain Spec';
is g(Spec::Alt.new), 'ALT', 'a Spec::Alt constraint accepts a Spec::Alt';
