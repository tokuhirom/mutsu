# ADR-0134: BEGIN-time effects run once, before the unit's run time, in a compiled per-compunit prologue over static-state lexicals

- Status: Accepted (2026-09-30; slice 1 implemented — see §7)
- Date: 2026-09-30
- Deciders: tokuhirom, Claude
- Addresses: [#9919](https://github.com/tokuhirom/mutsu/issues/9919)
  (`use Foo:if(EXPR)` evaluates `EXPR` at run time), and the general gap that
  [ADR-0026](0026-slang-activation-architecture.md) §2.1,
  [ADR-0087](0087-runtime-export-hook-parse-time-approximation.md) alternative 1,
  [ADR-0098](0098-if-pragma-actions-slang.md) §5 and
  [ADR-0124](0124-parse-time-export-probe-for-computed-export-stashes.md) §4
  each deferred as "the eventual answer, out of scope here"
- Related: [ADR-0041](0041-sub-hoisting-vs-compile-time-name-visibility.md)
  (BEGIN-time name visibility; its §9 rollback keeps working inside the
  prologue), [ADR-0133](0133-no-per-call-ast-compile-at-runtime.md) (the prologue
  is compiled bytecode, not a runtime AST walk)

## 1. Context

In Rakudo, a compilation unit is parsed and executed in lockstep. A `BEGIN`
block runs the moment the parser reaches its end. It runs once, whatever
encloses it: a sub that is never called, a loop, or a class body. It sees
every lexical in its *static* state: declared, holding its type's default,
with no run-time initializer applied yet. Whatever it stores there becomes the
lexical's starting value. `use` and `constant` are BEGIN-time effects of the
same kind.

mutsu has no BEGIN time. Each compunit (the program, a module, an EVAL
string) is parsed whole, then compiled, then run. A BEGIN is approximated in
three ways:

- **`run_toplevel_begin_phasers`** (`src/runtime/run_prelude.rs`) hoists a
  top-level BEGIN into a sub-interpreter run before the compile. It does this
  only when a Debug-string scan of the body finds no call, bareword or `use`.
- **`reorder_phasers`** (`src/runtime/phasers.rs`) moves a BEGIN earlier within
  its block. It stops at the first "barrier" statement, and it does not move a
  value-position BEGIN.
- **`BeginOnceExpr`** memoizes a value-form BEGIN per site. It runs the body at
  first *execution*, not at compile time.

The `:if(...)` of #9919 is not a BEGIN at all. It is read at run time, in
position.

Measured on rakudo 2026.07 against mutsu `main` (e2ab7eb7):

| program | rakudo | mutsu |
|---|---|---|
| `my $c = True; BEGIN say $c.raku` | `Any` | `Bool::True` |
| `my $x = 1; { my $x = 2; BEGIN say $x.raku }` | `Any` | `2` |
| `my @a = 9; BEGIN @a.push(1); say @a` | `[9]` | `[9 1]` |
| `my $c = 5; constant K = $c; say K.raku` | `Any` | `5` |
| `sub f { my $x = 1; BEGIN say $x.raku }; say "m"` | `Any`, `m` | `m` (the BEGIN never runs) |
| `for 1..3 { BEGIN say "b" }` | `b` once | `b` three times |
| `my $r = { BEGIN 42 }; say $r()` | `42` | `Nil` |
| `BEGIN say K; constant K = 3` | compile error, undeclared `K` | `(Any)` |
| `my $x will begin { $_ = 3 }; say $x` | `3` | dies: cannot assign to an immutable value |
| `use if; my $c = True; use Foo:if($c)` | compile error: `Did not provide compile-time-value for :if adverb in use statement` | loads `Foo` |
| `use if; use Foo:if(False); foo()` | compile error, undeclared routine `foo` | run-time `Unknown function: foo` |

Rakudo agrees with mutsu where the static state and the run-time state
coincide. That is why the gap stayed hidden:

| program | both |
|---|---|
| `my $c; BEGIN $c = 5; say $c` | `5` |
| `sub f { my $x; BEGIN $x = 5; say $x; $x++ }; f(); f()` | `5`, `5` |
| `for ^2 { my $v; BEGIN $v = 1; say $v; $v = 7 }` | `1`, `1` |
| `use if; BEGIN my $c = True; use Test:if($c)` | loads |
| `say 1; BEGIN say 2; CHECK say 4; INIT say 5; say 3` | `2 4 5 1 3` |

One more rakudo measurement pins what a fresh frame receives. A Scalar gets a
fresh container holding the static value; an `@`/`%` lexical binds the *same*
object:

```raku
sub f { my @a; BEGIN @a.push(1); @a.push(2); say @a }; f(); f()
# [1 2]
# [1 2 2]
```

## 2. Decision

### 2.1 The semantic contract

For every compunit (program, module, EVAL string):

1. **Every BEGIN-time effect runs exactly once**, before the unit's CHECK,
   INIT and mainline, **in source order**. The effects are:
   - `BEGIN` in statement and value form, at any nesting depth;
   - `constant` initializers;
   - `use` loads and their `:if(...)` conditions;
   - `will begin` traits.

   Nesting inside a routine, closure, loop, conditional or package body
   neither suppresses nor repeats an effect.
2. **Effects see lexicals in their static state.** Every lexical in scope is
   declared and holds its type's default, or whatever an earlier BEGIN-time
   effect stored in it. No run-time initializer (`my $c = True`) has run.
3. **What an effect stores is the lexical's static value.**
   - At unit level the mainline starts from it. A run-time initializer then
     overwrites it, so `my @a = 9` loses a BEGIN-time push; `my @a;` keeps it.
   - Each fresh frame of an enclosing routine or block starts from it too. A
     Scalar gets a fresh container holding the value; an `@`/`%`/`&` lexical
     binds the static object itself, as rakudo's pad clone does (§1).
4. **A value-form BEGIN's value is a constant of its site.** Executing the
   site reads it and never runs the body.
5. **A failed effect is a compile error.** It is raised as
   `X::Comp::BeginTime` before any CHECK, INIT or mainline code runs. The
   post-parse static checks (the undeclared-routine check and its siblings)
   run *after* the effects, which is the order rakudo reports them in (§1).
6. **`use Foo:if(EXPR)`, under the `if` pragma's mode (ADR-0098 §2.3),
   evaluates `EXPR` as a BEGIN-time effect:**
   - an undefined value is the compile error `Did not provide compile-time-value
     for :if adverb in use statement`;
   - `False` makes the statement empty: no load, no import, and the names the
     parse-time scan registered for it do not count as declared in the §2.1.5
     checks;
   - `True` is an ordinary `use`.

   This is what the vendored module's actions-role body does on rakudo.
   Stating it as the mode's meaning keeps ADR-0026 §4's rule that mutsu never
   runs Rakudo-internal bodies: the mode is registered because the real module
   declares it, and the behaviour is written down here, not guessed from a
   name.

### 2.2 The mechanism: a compiled prologue in the unit's own frame

The compiler emits a **BEGIN prologue** at the head of each compunit's code:
the unit's BEGIN-time effects, in source order, compiled into the unit's own
frame. The runtime runs the prologue, then the post-parse checks, then CHECK,
INIT and the mainline.

- **Unit-level lexicals.** The prologue shares the mainline's frame. It
  therefore reads and writes the mainline's own local slots, and no second
  environment exists to keep in sync. A declaration's run-time initializer
  stays at its source position. A declaration without an initializer does not
  reset a slot the prologue wrote.
- **A lexical of an inner scope** (a routine, closure or block body) that a
  nested effect touches gets a **static cell**. The cell is a unit-frame slot
  that the prologue addresses in place of the inner lexical. The inner
  declaration, on each frame entry, starts from the static cell as §2.1.3
  says, and then applies its own initializer if it has one. Nothing is
  allocated for a lexical no effect touches, so the common frame entry is
  unchanged.
- **A statement-position BEGIN** is compiled into the prologue only. Its
  source position compiles to nothing.
- **A value-form BEGIN** is evaluated in the prologue into its site's slot.
  The existing `site_id` / `once_store` pairing is reused: the prologue
  fulfills the key, and the site's `BeginOnceExpr` always finds it cached.
- **`constant`** is evaluated in the prologue. The compile-time folder
  (`const_fold.rs`) keeps inlining what it can prove, and the value it folds
  must equal what the prologue computes.
- **`use`.** The prologue performs a `use`'s load at its position among the
  effects. A BEGIN after a unit-level `use` therefore sees the module's
  imports. A `use` nested in a block keeps importing at its lexical position,
  and its load moves into the prologue, subsuming GH-8201's `PreloadModule`
  hoist.

The prologue is ordinary bytecode, compiled once per compunit. There is no
AST evaluation at run time (ADR-0133), and there is no sub-interpreter:
`Never build an Interpreter to run code` holds.

### 2.3 What is retired

Each of these is deleted, not kept as a fallback, in the slice that
supersedes it (§6):

- `run_toplevel_begin_phasers`, the sub-interpreter BEGIN hoist, together with
  its Debug-string hoistability heuristic;
- the BEGIN half of `reorder_phasers` / `reorder_phasers_for_eval`, and the
  BEGIN lifting in `lift_phasers_from_current_expr`. The CHECK/INIT ordering
  stays;
- the run-time `:if` guard around `UseModule` (`compiler/stmt.rs`);
- `PreloadModule` for nested `use` (GH-8201), once loads move into the
  prologue.

### 2.4 Not decided here: feedback into the parse

A BEGIN-time effect that changes *how the rest of the unit parses* is still
approximated. Examples: a `use` whose `sub EXPORT` computes names, a BEGIN
that installs an operator, a slang. The prologue runs after the parse, so the
parser still learns those names from the static scan (ADR-0087), the
parse-time probe (ADR-0124) and slang activation (ADR-0026).

Interleaving execution with the parse is the remaining step toward rakudo's
model. It needs a re-entrant compile-and-run inside a memoizing parser. It is
left to a later ADR, which this one does not block. Once it lands, the
prologue becomes the order in which that interleaving already ran, and those
three approximations are deleted.

## 3. Consequences

- The eleven divergences of §1 close, each pinned by a `t/` test that also
  runs green under `raku`.
- A BEGIN inside a routine that is never called now runs. A program that
  relied on mutsu *not* running it was relying on a bug.
- **Constants move from run time to BEGIN time.** A constant whose
  initializer reads a run-time-assigned lexical changes value (`Any` instead of
  the assigned value). That is rakudo's answer, and roast is the arbiter.
- **Module BEGINs run on every load.** Rakudo runs a precompiled module's
  BEGINs once, at precompilation. mutsu has no bytecode precompilation (its
  precomp caches the AST only), so it behaves like an uncached rakudo load.
  This stays an accepted divergence until precompiled bytecode exists.
- Error timing changes. Everything the prologue raises surfaces before the
  mainline prints anything, which is rakudo's order.

## 4. Alternatives considered (rejected)

- **Evaluate only the `:if` expression at parse time** (#9919 alone). This
  would give exact parity for the issue's example. But a condition that is
  BEGIN-dependent in a legitimate way (`BEGIN my $c = True; use Foo:if($c)`)
  cannot be decided without running BEGINs. It would then need a "fall back to
  run time" branch. That is two mechanisms for one question, and ADR-0124 §4
  already rejected this shape ("grows into a second evaluator").
- **Run BEGIN-time effects in a separate interpreter or environment**, as
  `run_toplevel_begin_phasers` does today, and copy values back. Every copy
  rule is a place to drift: which lexicals, which containers, and what a
  closure captured. The shared frame (§2.2) has no copy step at all.
- **Keep extending the reorder pass.** Moving statements within a block
  cannot express "once, regardless of the enclosing routine or loop", or
  "before the initializer that precedes it in source". Each of the §1 rows
  would need its own exception to the barrier rule.
- **Full parse-interleaved execution now.** This is the complete answer, and
  §2.4 records it as the next step. It is not the first one: the value
  semantics above are independent of it, they cover every divergence measured
  in §1, and they are what the interleaved design would execute anyway.

## 5. Open questions

- Should `will begin` and the other trait-level BEGIN-time effects (`is
  export` trait bodies, a `trait_mod` declared in the unit) all be enumerated
  as prologue effects in slice 3? Rakudo runs every `trait_mod` at BEGIN time,
  and mutsu applies most of them at declaration time.
- EVAL's prologue runs when the EVAL runs, which is the EVAL string's compile
  time on rakudo too. A BEGIN in an EVAL nested in a BEGIN therefore needs no
  special case. This will be verified by `roast/S04-phasers/in-eval.t`.

## 6. Implementation plan

1. **Unit-level prologue.** Statement-form BEGIN directly in the unit
   (program, module, EVAL). Unit lexicals are in their static state, and the
   post-parse checks run after the prologue. Retires
   `run_toplevel_begin_phasers` and the top-level BEGIN handling of the
   reorder passes.
2. **Nested and value-form effects.** BEGIN inside routines, closures, loops
   and package bodies, with the §2.2 static cells, and value-form BEGIN at any
   depth through the pre-fulfilled site slot. Retires the rest of the BEGIN
   half of the reorder passes.
3. **`constant`, `use` and `:if` as prologue effects.** Closes #9919 and
   retires the `:if` run-time guard and `PreloadModule`.

Each slice keeps every whitelisted `S04-phasers/*.t` green and records its
status here.

## 7. Implementation status

**Slice 1 — implemented** (`src/runtime/begin_prologue.rs`,
`t/control/begin-prologue-static-state.t`).

- The prologue is produced as an AST partition of the unit's top level,
  before the unit's single compile. The prologue and the run-time remainder
  are then compiled together, so the prologue runs in the unit's own frame, as
  §2.2 requires. The mainline and EVAL take the partition as the first step of
  `phasers::reorder_phasers`. A module takes it on its own
  (`begin_prologue::order_unit`), because a module's top level gets no other
  reordering.
- The partition covers the unit up to its last top-level statement-form
  `BEGIN`. Within that prefix, the prologue takes:
  - the `BEGIN`s;
  - every declarator (`use`, routines, packages including a `unit` marker,
    types, `constant`);
  - the static half of each variable declaration.

  The initializers stay in place as assignments. Nothing after the last
  `BEGIN` moves, because no BEGIN observes it. Slice 3 removes this bound for
  `use` and `constant`.
- When the mainline's undeclared-routine check fails, the prologue alone runs
  first (`run_begin_prologue_only`), so the BEGIN's output precedes the
  compile error, as on rakudo.
- **Residue until slice 2:**
  - A package body moves whole, so a bare run-time statement inside a class
    or `module Foo { ... }` body that precedes a unit-level BEGIN runs with
    the prologue instead of in its source position.
  - Value-form BEGIN and nested BEGIN keep their pre-ADR handling
    (`BeginOnceExpr`, the nested reorder rules).
