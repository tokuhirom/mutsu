use Test;

# ADR-0019 E7 step 6: `.^methods()` (`Interpreter::dispatch_classhow_methods`,
# `src/runtime/methods_classhow_builtin_methods.rs`) already extracted a
# runtime-mixed-in role's method names (`(5 but R)`-style values) and folded
# them into `.^methods(:local)`'s output, but the default (non-`:local`)
# branch never did the same -- so `(5 but R).zork` was callable via ordinary
# dispatch, yet absent from `(5 but R).^methods` (no `:local`). Confirmed
# against real `raku`: `(5 but R).^methods.map(*.name)` includes `zork`
# because a `but` mixin's `.^mro` puts an anonymous composite pun class
# (`Int+{R}`) FIRST, ahead of the base type -- `((Int+{R}) (Int) (Cool) (Any)
# (Mu))` -- and that pun class's own methods are exactly the mixed-in role's
# methods.

plan 9;

role R1 { method zork { "zork!" } }

my $x = 5 but R1;
ok "zork" (elem) $x.^methods.map(*.name), 'mixed-in role method appears in .^methods (no :local)';
ok "zork" (elem) $x.^methods(:local).map(*.name), 'mixed-in role method still appears in .^methods(:local)';
is $x.zork, "zork!", 'the mixed-in role method actually runs';

# A second role method, to confirm both are collected, not just the first.
role R2 { method quux { "quux!" } }
my $y = 5 but R1 but R2;
ok "zork" (elem) $y.^methods.map(*.name), 'first mixed-in role method found with two roles mixed in';
ok "quux" (elem) $y.^methods.map(*.name), 'second mixed-in role method found with two roles mixed in';

# The base type's own (builtin) methods are still present alongside the
# mixed-in role's methods -- the fix must not have replaced the base walk.
ok "abs" (elem) $x.^methods.map(*.name), 'base Int type still contributes its own methods';

# A value with no mixed-in role is unaffected (regression guard for the
# ordinary, non-mixin case this box's shadow-check also covers).
class A1 { method foo { "A1::foo" } }
class B1 is A1 { method bar { "B1::bar" } }
my @names = B1.^methods.map(*.name);
ok "foo" (elem) @names, 'plain inheritance: ancestor method still found without any mixin';
ok "bar" (elem) @names, 'plain inheritance: own method still found without any mixin';

# `:all` still works together with a mixed-in role.
ok "zork" (elem) $x.^methods(:all).map(*.name), 'mixed-in role method still appears with :all';

done-testing;
