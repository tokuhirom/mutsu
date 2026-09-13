use v6;
use Test;

# Evaluating a parameter default must not cost the routine its `&`-sigil
# lexicals.
#
# A default expression is evaluated re-entrantly (`eval_param_default` ->
# `eval_block_value`), and that carrier snapshots the env's code-var entries on
# entry so a block-local `sub`/`my &f` cannot leak out of it. The snapshot only
# sees this tier's overlay, but a nested call inside the default can FLATTEN the
# env chain and migrate the caller's own `&`-bindings into that tier — and the
# restore then dropped them as if the block had added them, with no parent tier
# left for them to shadow back through.
#
# So any routine with a default that had to be computed re-entrantly (anything
# beyond a literal: `$x.keys` on a Hash, a method call, ...) lost every `&`
# lexical visible to its body. That hit `use JSON::Fast` hardest, because its
# `EXPORT` sub hands `&to-json` over as a pure lexical rather than a package
# symbol: `sub assert-sorted($obj, @keys = $obj.keys) { to-json($obj) }` died
# with "Unknown function: to-json" (#8226).

plan 6;

my &f = -> $n { "f($n)" };

sub plain-default($x, @keys = (1, 2)) { f(1) }
is plain-default(1), 'f(1)', 'a literal default keeps the & lexical';

sub computed-default($x, @keys = $x.keys) { f(2) }
is computed-default({ a => 1 }), 'f(2)',
    'a default computed from a parameter keeps the & lexical';

sub scalar-default($x, $k = $x.keys) { f(3) }
is scalar-default({ a => 1 }), 'f(3)', 'the same for a scalar parameter';

sub nested-caller($x, @keys = $x.keys) { indirect() }
sub indirect() { f(4) }
is nested-caller({ a => 1 }), 'f(4)',
    'a routine called from such a body keeps it too';

# The binding the default evaluation had to rescue is the real one, not a stale
# copy: rebinding it afterwards is visible.
&f = -> $n { "g($n)" };
is computed-default({ a => 1 }), 'g(2)', 'and it is the live binding, not a copy';

# The leak the snapshot exists to prevent must still be prevented: a `sub`
# declared inside a block stays lexical to that block.
{
    sub block-local() { 1 }
    block-local();
}
# Looked up dynamically: `&block-local` written literally here is a compile-time
# "Undeclared routine" in rakudo, which is the stricter half of the same rule.
nok (try ::('&block-local')).defined, 'a block-local sub still does not leak out';

# vim: ft=perl6
