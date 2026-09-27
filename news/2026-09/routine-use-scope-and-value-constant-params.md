# A routine's `use` stays in the routine; value constants work as parameters

Three dispatch fixes found by working the Bitcoin distribution's `t/basics.t`,
which died with `Cannot resolve caller Numeric(secp256k1::Point:D: )`.

**A `use` inside a routine body leaked its operators into the whole package.**
An operator imported by a routine-body `use` is aliased under the routine's
unit package (`secp256k1::infix:<**>`). A module's own definitions have the
same key shape, so `pop_import_scope` kept the alias when the routine returned.
`secp256k1` does `use FiniteField` inside `Point`'s methods. The `our constant G`
constructs a `Point` while the module loads. After that, every `**` in the
module used FiniteField's modular `infix:<**>`, including the
`where 1 < $n < 2**256` guards on its `infix:<*>` candidates. The pop now also
drops keys recorded as imported routine aliases during that scope.

**A constant bound to a value can stand where a parameter type goes.** Rakudo
compiles `multi f(G)` (with `constant G = Point.new(...)`) and
`sub f(TAU $x)` to the constant's type plus a smartmatch against it. For a
definite object that smartmatch is `===`, WHICH identity. mutsu had three gaps:
- It rejected the name as an invalid typename in a script.
- It ranked the candidate below a plain `Point:D` one.
- It cached the multi's verdict per argument type, so `f(5)` and `f(6)` shared
  one answer.

The binder, the ranking and the resolution-cache gate now treat such a
constant as a value constraint.

**A `where` clause ran once per retry.** When no user candidate binds,
`resolve_function_with_types` retries with wider candidate sets, and each set
re-gathered the candidates already tried. A declined user `infix:<*>` ran its
`where` three times per multiplication; rakudo runs it once. Candidates that
failed to bind are now skipped in the later passes.

Bitcoin's `t/basics.t` now computes correct results. It is still far too slow
to finish, because the script's imported `infix:<*>` candidates are visible
inside `FiniteField`'s routines. That is #9944.

Pinned by `t/modules/module-routine-use-import-stays-lexical.t`,
`t/routines/signature/value-constant-as-param-constraint.t` and
`t/routines/dispatch/multi-where-runs-once-when-nothing-binds.t`.
