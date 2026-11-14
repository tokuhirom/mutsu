# A slang package declarator that parses its own body runs on the slang host

Grammar::BNF's `bnf-grammar A::B { <foo> ::= "bar" };` now declares a working grammar, as it
does on rakudo: all five of the distribution's test files (`t/01`..`t/05`) produce output
byte-identical to rakudo's, where `t/03-slang` passed 0 of 16, `t/05-precomp` 0 of 2 and
`t/04-abnf` died after 10 of 29.

This is slice 1 of ADR-17741 (accepted on 2026-11-13). A slang's `package-declarator`
candidate that does not call the core `<package_def>` is now a production hook, like a
`statement-control` one (ADR-15762); one that does call it keeps ADR-0091's declarative read. The
pieces it needed:

- **Named languages.** `$*LANG.define_slang('Grammar::BNF', Grammar::BNF, Grammar::BNF-actions)`
  records the grammar under its name in the slang host. The host grammar's `FOREIGN-LANG($name,
  $rule)` (and `FOREIGN_LANG`) parses `$rule` with that grammar and its actions where the
  production's cursor stands.
- **The resolver.** `$*R.current-package` and `$*R.get-global` answer a scratch package on the
  host and the real packages in the unit. `merge-generated-lexical-declaration` of a
  `RakuAST::Declaration::Import` declares a lexical. Its value comes from the unit's run of the
  action.
- **Two replays, in order.** The unit first re-creates the types the hook minted outside the
  production's own action (the grammar `FOREIGN-LANG` built). It then re-runs the production's
  action against its real packages, with every host type in the recorded Matches mapped to the
  unit's. Types the action itself minted (stub packages, the host's scratch package) are left to
  the action replay. Actions of the foreign language are no longer recorded as the slang's own.

Fixing the real module also turned up general bugs, each fixed here:

- `$t := do { my $s := Str; $s }` assigned into whatever `$t` was bound to, because the block's own
  stores consumed the rebind's markers.
- A grammar method that returns another grammar's Match over the same target
  (`Other.subparse(self.orig, :pos(self.pos))`) was read as an offset from the call, not a
  position.
- A type minted by `new_type` under a qualified name made its name's packages appear in stashes
  (`Deep.WHO.EXISTS-KEY('Down')` on a fresh stub).
- `M::Nested::In` did not resolve when a stash binding had put a package named `Nested` under `M`.
- A user subclass of a RakuAST node class refused its parent's positional constructor arguments.
- A `Declaration::Import` built with `.new` had no `compile-time-value`.

`--dump-ast`, `--dump-bytecode` and the analysis API still run none of this code: a new test pins
that neither the slang module, nor the foreign language's actions, nor the production's action
runs on those paths.
