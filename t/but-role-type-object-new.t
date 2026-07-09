use v6;
use Test;

# `TypeObject but Role` creates a composed subtype (`Base+{Role}`) whose `.new`
# constructs a base instance with the role mixed in. mutsu previously wrapped the
# type object in a value-level Mixin whose `.new` fell through to the
# "Unknown method value dispatch ... new" error. Regression for zef's
# `distribution-depends-parsing` recommendation-manager mock
# (`Zef::Repository but role :: { ... }).new(:backends[])`).

plan 16;

class Base { has $.x; method greet { "base $.x" } }

# --- named role, methods only ---
role R { method extra { 99 } }
my $t = (Base but R).new(:x(5));
ok $t.defined, 'composed type-object .new returns a defined instance';
is $t.x, 5, 'base attribute initialized via .new';
is $t.greet, 'base 5', 'base method works on composed instance';
is $t.extra, 99, 'role method works on composed instance';
ok $t ~~ R, 'composed instance does the role';
ok $t ~~ Base, 'composed instance isa the base class';

# --- anonymous role ---
my $a = (Base but role :: { method anon { 'A' } }).new(:x(1));
is $a.anon, 'A', 'anonymous role method works';
is $a.x, 1, 'base attribute with anonymous role';

# --- role with its own attribute + default ---
role Tagged { has $.tag = 'default'; method describe { "tag:$.tag" } }
my $g = (Base but Tagged).new(:x(7));
is $g.tag, 'default', 'role attribute default initialized';
is $g.describe, 'tag:default', 'role method reads role attribute';
is $g.x, 7, 'base attribute alongside role attribute';
ok $g ~~ Tagged, 'does the attribute-bearing role';

# --- two roles composed (parenthesized: `but` is non-associative) ---
role A { method a { 'a' } }
role B { method b { 'b' } }
my $ab = ((Base but A) but B).new(:x(9));
is $ab.a, 'a', 'first role method';
is $ab.b, 'b', 'second role method';
is $ab.x, 9, 'base attribute with two roles';
ok ($ab ~~ A) && ($ab ~~ B), 'does both roles';
