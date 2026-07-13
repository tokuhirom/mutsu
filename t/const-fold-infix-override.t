use v6;
use Test;

# Pin for the constant-folding safety condition (ADR-0006 §2.1): a user-defined
# `infix:<op>` overrides even native `Int + Int`, so declaring one anywhere in
# the compilation unit must disable folding for the whole file — including for
# expressions that appear *before* the declaration, since mutsu registers the
# operator globally.

plan 5;

# Compiled before the declaration below, yet the override must still win.
is 1 + 2, 99, 'a literal-only expression before the declaration is not folded';

sub infix:<+>($a, $b) { 99 }

is 3 + 4, 99, 'a literal-only expression after the declaration is not folded';
is 2 + 3 * 4, 99, 'a nested literal expression is not folded either';

# Operators that are NOT overridden still fold, and still produce the right value.
is 6 * 7, 42, 'an unrelated operator keeps working';
is 'a' ~ 'b', 'ab', 'concatenation keeps working';

done-testing;
