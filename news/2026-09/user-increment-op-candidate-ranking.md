# The native `++`/`--` implementations became dispatch candidates

A user `multi prefix:<++>` used to take over the operator wholesale:

```raku
multi prefix:<++>($a) is default { $a - 1 }
my $foo = 1;
say ++$foo;      # rakudo: 2 (the core Int:D candidate)   mutsu: 0 (the user's)
```

In Raku an operator is a `multi`, and `&prefix:<++>` ships nine core candidates
(`Mu:D`, `Mu:U`, `Int:D`, `int`, `uint`, `Bool`, `Num:D`, `Num:U`, `num`).
Declaring one adds a tenth; it does not replace the operator. Since an untyped
parameter is `Any` — narrower than `Mu` but wider than `Int:D` — the user
candidate correctly wins for `Rat`, `Str`, a user class and an undefined `Any`,
and correctly *loses* for `Int`, `Bool` and `Num`.

`is default`, which the ticket originally named as the mechanism, was never
involved: the `is default` tie-break only runs when two or more candidates tie,
and with a single user candidate it was never reached.

## What was actually wrong

Two layers, plus two more bugs in the same neighbourhood:

1. **The dispatch decision was made at parse time, with no argument types.** With
   a `prefix:<++>` sub in scope, `prefix_expr` rewrote `++$foo` into
   `Expr::Call { name: "prefix:<++>" }` and the `PreIncrement` opcode was never
   emitted, so the native increment was out of the picture entirely.
2. **The native operator was not a candidate.** `resolve_function_with_types`
   ranks only registered `FunctionDef`s; the native `++` was a hard-coded arm in
   `call_function_fallback`, reached only *after* a failed user resolution. There
   was nothing for the user's `($a)` candidate to lose a comparison against.
3. **The native fallback did not mutate.** With a *non-matching* user candidate
   in scope, `"++" => arith_add(arg, 1)` returned a fresh value and left the
   variable alone: `multi prefix:<++>(Str $a) {...}; my $i = 1; say ++$i; say $i`
   printed `1` / `1` where rakudo prints `2` / `2`.
4. **`postfix:<++>` / `postfix:<-->` never consulted a user candidate at all.**
   `parse_postfix_update_op` ran before the user-op matcher, so
   `multi postfix:<++>($a) { "USER" }; my $s = "abc"; say $s++` printed `abc`.

## What shipped

[ADR-0071](../../docs/adr/0071-native-operators-are-dispatch-candidates.md):
**a natively implemented operator participates in its own multi dispatch as an
explicitly modelled candidate set, and the decision is made at the call site that
owns the lvalue — at run time, with the argument in hand.**

- `src/runtime/native_increment_dispatch.rs` models the core candidate set as the
  constraint an argument binds to (`Int` when definite, `Bool` and `Num` at
  either definiteness, `Mu` otherwise) and ranks it against the winning user
  candidate with the very metrics multi dispatch already uses —
  `candidate_specificity_rank_for_args` for nominal narrowness and refinement,
  then `candidate_type_distance`. Ties go to core, matching rakudo.
- The `CallFunc` opcode grew a name-gated guard: when the core candidate wins,
  the increment runs there, through the `VarRef` the call-argument compiler
  already wrapped the operand in, and stores through
  `store_core_increment_result` — reaching all ten lvalue shapes (plain scalar,
  array/hash/nested element, attribute, `state`, `our`, native `my int`, typed
  `my Int`, sigilless alias). When a user candidate wins, the call proceeds and a
  non-`is rw` candidate leaves the variable alone, as rakudo does.
- The postfix parse site now emits the same call when a user `multi postfix:<++>`
  or `postfix:<-->` is declared, making the two directions symmetric.
- A plain `sub prefix:<++>` (not a `multi`) stays a lexical shadow and replaces
  the operator outright for every type, as in rakudo.
- The sigilless-alias propagation that the pre-increment and pre-decrement
  opcodes each carried inline is now one shared
  `propagate_incdec_sigilless_alias`, so the operator has one store path reached
  from both the opcode and the call site.

No `OpCode` variant changed, so the `opcode_size_guard` test is untouched, and
nothing on the hot path moves: with no user candidate in scope the dedicated
increment opcodes are still the only path.

## Pin

`t/user-increment-op-candidate-ranking.t` — 47 rows covering all four operators,
every argument type the core set distinguishes, seven user parameter shapes
(untyped, `Any`, `Mu`, `Cool`, `Int`, `Int:D`, `where`, `subset`, `is rw`), the
non-`multi` shadow, and the no-user-candidate baseline. It passes **identically
under `raku` and under `mutsu`**, so it is a rakudo-verified oracle rather than a
transcription of mutsu's behaviour.

## Left open

The same missing gate exists for infix operators — `try_user_infix` hands every
matching user candidate the call — and is tracked in
`todo/deep/user-infix-candidate-beats-core-operator.md`.
