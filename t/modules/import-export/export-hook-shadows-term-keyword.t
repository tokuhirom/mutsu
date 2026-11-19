use v6;
use Test;

# `True`/`False`/`Nil`/`Empty`/`Any` are ordinary CORE-scope LEXICAL bindings in
# Raku, not syntax keywords, so a `use` that exports same-named symbols
# legitimately shadows them for the rest of the importing file. mutsu matched
# them as hardcoded literals in `keyword_literal` and folded them to an
# `Expr::Literal` at parse time, which no run-time import could ever reach --
# `say True` printed the Bool no matter what the module installed.
#
# The ecosystem `Logic::Ternary` distribution replaces exactly these names with
# three-valued-logic objects, and computes the NAMES it installs from the `use`
# arguments, so no static scan of the module can know them. The fix therefore
# defers the choice to run time, but only inside a compunit that actually `use`d
# a module with a `sub EXPORT` hook: everywhere else `True` stays a folded
# compile-time constant.
#
# https://github.com/tokuhirom/mutsu/issues/9047

plan 15;

use lib 't/lib';
use ExportHookShadowsTermKeyword;

is True.Str, 'Tri(1)', 'the bare `True` term reads the value the EXPORT hook installed';
is False.Int, -1, '... and so does `False`, with the hook object answers, not Bool ones';
is True.^name, 'Tri', 'the shadowed term really is the imported type, not a coerced Bool';

my $t = True;
is $t.Int, 1, 'the shadowed term survives being bound to a variable';

ok True ~~ Tri, 'the shadowed term smartmatches the imported type';
nok True === Bool::True, 'the shadowed term is NOT the Bool enum value any more';

# The QUALIFIED spelling is a package lookup, never the bare term keyword, so
# the real Bool enum values stay reachable exactly as in Rakudo.
is Bool::True.gist, 'True', '`Bool::True` still names the real Bool enum value';
is Bool::False.Int, 0, '... with the real Bool answers';

# The hook installed only `True` and `False`. The other three shadowable term
# keywords are parsed the same deferred way in this (tainted) compunit, so this
# is the fallback path: nothing shadows them, and they must still be the CORE
# constants they were folded to before.
nok Nil.defined, '`Nil` is unshadowed in the same compunit and keeps its CORE value';
is Empty.elems, 0, '`Empty` likewise';
nok Any.defined, '`Any` likewise';

# A routine hot enough to be JIT-compiled reads the term through the same
# run-time check (#18211: `GetShadowableTerm` is a JIT step op).
sub hot-false() { False }
sub hot-nil() { Nil }
my $hits = 0;
my $nils = 0;
for ^3000 {
    $hits++ if hot-false().Int == -1;
    $nils++ unless hot-nil().defined;
}
is $hits, 3000, 'a hot routine still reads the hook-installed `False`';
is $nils, 3000, 'and the unshadowed `Nil` fallback';

# An attribute default names the shadowed term too: the constructor must
# evaluate it rather than store the Bool it would otherwise fold to (#16897).
class WithDefault { has $.f = False; has $.n = Nil }
sub make-defaults() { my @o; @o.push: WithDefault.new for ^200; @o }
my @made = make-defaults();
ok @made.grep(*.f.Int == -1) == 200, 'a hook-installed `False` is an attribute default';
nok @made[0].n.defined, 'an unshadowed `Nil` default still resets the attribute';
