# A `while`/`until`/`repeat` body is a Block, invoked with the raw condition

ADR-0048's block-invocation contract is now implemented for the loop
constructs. `while 42 { say $^c }` prints `42` (it printed `True`),
`until False { say $^c }` prints `False` (it printed `True`), and
`{ while 42 { $^c } }.arity` is `0` (it was `1`) — all matching real `raku`.
This closes ADR-0048 Phase 4 (D4 + D5) and the deep finding
`todo/deep/placeholder-scope-loop-while-block-boundaries.md`, whose remaining
open items after the 2026-09-01 re-verification were exactly these.

## The rule

Every `{ ... }` body is a Block that its construct invokes with some number of
arguments. For a prefix `while`/`until` that is **one** argument, the *raw*
(un-boolified) value of the condition **as written**, re-supplied on every
pass. `repeat {} while/until` invokes its body once before the condition has
ever run, so its first pass is supplied `Mu` and every later pass the condition
value. A `while`/`until` statement *modifier* introduces no block at all, so
its placeholders stay the enclosing routine's own parameters.

## Three things the implementation needed that the design did not predict

**The bind and the arity check have to live in different bytecode regions.**
Phase 3's `emit_inlined_body_placeholder_binds` emitted both together, which
works when a construct evaluates its supplied value once, right before the
body. A loop re-evaluates its condition every pass, so the bind belongs in the
*condition* region — `Dup` the value there and bind the copy, and the loop op
still finds exactly one value at the end of its range, leaving the stack
contract untouched. The arity failure, though, is raised on *invocation*:
`while 0 { "$^a $^b" }` must stay silent. So the emitter was split into
`emit_inlined_body_placeholder_bind_value` and
`emit_inlined_body_placeholder_arity_die`, and the loop arms place the two
halves independently.

**`Stmt::While` needed an `is_statement_modifier` flag**, mirroring
`Stmt::If`/`For`/`Given`. Without it, `while COND { $^a }` (a boundary supplied
the condition) and `say "$^a" while COND` (no block — `sub f { say "$^a" while
$i++ < 2 }; f(7)` prints `7` twice in raku) are literally the same AST node.
The flag also retires a false negative Phase 3 accepted on purpose: a block
genuinely nested inside a prefix loop's braces (`while 42 { { $^a } }`) is a
*second*, zero-argument Block, and now raises raku's "Too few positionals
passed; expected 1 argument but got 0".

**`until` needed an `is_until` flag, because raku supplies the condition as
written.** The parser lowers `until COND` to `while !COND`, so the only value
on the stack is the boolified negation — but `until False { $^c }` prints
`False` in raku, and `until 0 { $^c }` prints `0`. The compiler now compiles
the *inner* expression, binds a `Dup` of it, and re-applies `Not` to the copy
the loop tests. The flag is unavoidable: a hand-written `while !$x { $^c }`
genuinely does supply the negation and is AST-identical to the desugar.

D5 ("an explicit signature wins over a placeholder") turned out to belong in
the **parser**: `while COND -> $x { }` is desugared into a `VarDecl` plus a
`While` over an `AssignExpr` long before codegen, so `while_until.rs` and
`loop_repeat.rs` call the existing `placeholder_overrides_signature_error`
while the pointy parameter is still in hand.

## Role bodies: the scope half, and why the value half was left

ADR-0048 D7 also claimed `role R { $^c }` is legal in raku and that mutsu
over-rejects it. Both are true, and `Stmt::RoleDecl` is now a placeholder
boundary (`Signature(ArgSupply::AllMu)`), which fixes the half that was a real
mis-compilation: `{ role R { $^c } }.arity` was `1` and is now `0`.

Supplying the value was attempted and backed out. Re-auditing rakudo showed
D7's "the body runs at composition with `Mu`" is not what happens: the
parameter is an **uninitialized `VMNull` register**. It gists as `(Mu)` and
`$^c === Mu` is `True`, but `$^c.^name` reports `VMNull` and `$^c.defined`
*throws*; a body declaring two placeholders gets that same null for both rather
than an arity failure. On top of that, mutsu cannot easily supply anything
there: `add_role_decl_plan` turns a role body into `DeferredBodyOp`s that
`run_role_body_for_composition` recompiles **one statement at a time**, so a
`my $^c = Mu` prepended to the body becomes its own compilation unit and
neither scopes the name into the later statements nor stops `compile_unit`'s
mainline-placeholder check firing on them. A corpus scan of `roast/`,
`modules/`, `vendor/` and `lib/` found zero real uses. The over-reject stays,
recorded in `todo/deep/role-body-placeholder-mu-supply.md` and pinned — along
with the arity fix — in `t/placeholder-scope-rejecting.t`.

## Verification

`t/placeholder-scope-loop-condition.t` (21 cases) passes **unmodified under
real `raku`** as well as under mutsu, which is the standard the other two
ADR-0048 pin files already set. Beyond `make test` (36628 tests), a corpus scan
for placeholders used directly in loop bodies found no pre-existing hits, a
136-file targeted roast sweep passed, and the bundled-library gate
(`scripts/battery-testsuite.sh`, 289/312) was re-run — the Phase-2 DBIish
regression is why that gate is now part of this ADR's acceptance rather than
just CI's.
