# Every method of a composed role reads one binding of its `my constant`

A role body's `my constant` has two instances in a module: the one the declaring
unit evaluated (kept in the module-scope table) and the one the role's composition
bound ([#16255](https://github.com/tokuhirom/mutsu/issues/16255)). A plain role
method read the composed one, but a method that runs in a module frame (any method
with a `use` in it) read the declaring unit's, so two methods of the same role
compared against different objects.

Hash::Util's `LockedHash` stores a `HIDDEN` sentinel in `lock_keys` and compares
against it in `EXISTS-KEY` (which has `use nqp`): `%h<c>:exists` stayed `True` for a
hidden key and `hidden_keys` was empty. `t/lock_keys.t` of Hash::Util 0.0.5 now gives
34/34 as `raku` does (it was 27), and all six of its test files match.

The bareword read of a sigil-less constant in `push_bare_word_value_in` now prefers
the role's composed binding (`package_lexicals`, written by
`persist_role_body_lexicals`) over the module-scope table when the running frame's
lexical package is that role.

Not changed: a role body's constant is still evaluated once more per composition,
so two different classes composing one role still get two instances (rakudo shares
one). A first attempt to skip the composition-time run broke the role-constant
tests that depend on it; the remaining work is on the issue.

Regression test: `t/oo/role/role-body-constant-one-binding-across-methods.t`, which
loads a module file, where the divergence reproduced.
