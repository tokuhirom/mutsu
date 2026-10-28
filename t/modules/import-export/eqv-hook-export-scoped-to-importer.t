use Test;

# GH-15203 (Red t/06-optimizer): the `&infix:<eqv>` family a `sub EXPORT` hook
# installs belongs to the importing unit. A role module that imported nothing
# (and declares an `infix:<eqv>` candidate of its own) keeps the core `eqv`.
plan 7;

use lib 't/lib';
use EqvHookOps;

my $o = EqvHookImpl.new;

is $o.same(EqvHookAst.new, EqvHookAst.new), True, 'the role method sees core eqv, not the importer\'s hook family';
is $o.same("a", 1), 'LOCAL', 'its own candidate still applies';
is $o.same-where(EqvHookAst.new, EqvHookAst.new), 'where ok', 'also inside a multi candidate where clause';

# In a unit that declares its own `infix:<eqv>`, the operator is the full core
# `eqv`: two objects with equal user `.raku` are equivalent whatever their state.
class Tagged { has $.v; method raku { "tagged" } }
is $o.same-raku(Tagged.new(v => 1), Tagged.new(v => 2)), True, 'equal user .raku makes distinct objects eqv in a unit declaring eqv';
is (Tagged.new(v => 1) eqv Tagged.new(v => 2)), True, 'as in any other unit';
is $o.same(1, 1), True, 'plain values';
is $o.same(1, 2), False, 'plain values differ';
