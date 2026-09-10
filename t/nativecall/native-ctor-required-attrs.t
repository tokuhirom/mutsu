use Test;

# VM-native default construction extended to `is required` attributes (ledger
# §1 / phase ③). A provided required attribute is built natively (with the same
# type check as any typed `$`); an unprovided required attribute falls through
# to the interpreter, which raises X::Attribute::Required. These assertions
# match `raku`.

plan 14;

class P { has Int $.x is required; has Int $.y is required; has $.z = 9 }
class Reqd { has $.a is required; has $.b is required }
class R { has Int $.n is required }
class S { has @.items is required; has %.opts is required }
class Base { has $.id is required }
class Child is Base { has $.name = "?" }

# all required provided -> native build
my $p = P.new(x => 1, y => 2);
is $p.x, 1, 'required attr x provided';
is $p.y, 2, 'required attr y provided';
is $p.z, 9, 'non-required default alongside required attrs';

# a missing required attribute dies (X::Attribute::Required)
my $ok = Reqd.new(a => 1, b => 2);
is $ok.a, 1, 'both required provided builds';
is $ok.b, 2, '... and keeps b';
dies-ok { Reqd.new(a => 1) }, 'one missing required attr dies';
dies-ok { Reqd.new }, 'all required missing dies';

# required + typed: provided wrong type still dies
is R.new(n => 5).n, 5, 'required typed attr provided & matches';
dies-ok { R.new(n => "x") }, 'required typed attr wrong type dies';
dies-ok { R.new }, 'required typed attr missing dies';

# required @ / % attributes (provided -> native build). Note: the interpreter
# only enforces `is required` for `$`-scalar attributes, so a *missing* required
# `@`/`%` attribute does not die here (a pre-existing raku divergence); the
# native path matches the interpreter by falling through for any missing
# required attribute.
my $s = S.new(items => (1, 2, 3), opts => { k => 1 });
is-deeply $s.items, [1, 2, 3], 'required @ attr provided';
is $s.opts<k>, 1, 'required % attr provided';

# inheritance: required attr in a parent
my $c = Child.new(id => 42);
is $c.id, 42, 'inherited required attr provided';
dies-ok { Child.new }, 'inherited required attr missing dies';
