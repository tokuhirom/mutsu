# ADR-0134: BEGIN-time effects run once, before the unit's run time, in a compiled per-compunit prologue over static-state lexicals

- Status: Accepted (2026-09-30; slices 1–3 implemented — see §7)
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

**Slice 1 — implemented** (`src/runtime/begin_prologue/mod.rs`,
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
- A statically named `require` in that prefix adds a stub `package Foo {}`
  to the prologue. Rakudo installs that stub at compile time, and the load
  still happens at run time.
- The compiler's `use Test` hoist (`hoist_test_use_decls`) no longer moves
  the module above a BEGIN that precedes it.
- When the mainline's undeclared-routine check fails, the prologue alone runs
  first (`run_begin_prologue_only`), so the BEGIN's output precedes the
  compile error, as on rakudo.
- **Package bodies** (#10332, closing the slice's original residue). A
  class, grammar or brace-scoped `module`/`package` declaration the prologue
  takes is split (`src/runtime/begin_prologue/package_body.rs`,
  `t/modules/begin-prologue-package-body.t`):
  - the prologue keeps the declaration with its BEGIN-time part: attributes,
    methods, subs, nested types (themselves split the same way), `use`,
    phasers, and the static half of each `my`/`our` variable;
  - the bare statements and the initializers (as assignments) stay at the
    declaration's position as a `Stmt::PackageRuntimeBody`. It compiles to
    the same `PackageScope` a `package Foo { ... }` body runs in, which
    re-enters the package. The body's `my` lexicals are bound from the
    package's static store (`package_lexicals`, where its methods already
    read them) for the duration, written back on exit, and a same-named
    outer lexical is restored. `$?CLASS` is bound to the class.

  A `state`, dynamic, exported or `&` variable keeps its declaration whole,
  and so does a group declaration with an initializer (`my ($a, $b) = ...`),
  so those initializers still run with the prologue. A role body is not
  split: it runs at composition.

**Slice 2 — implemented** (`src/runtime/begin_prologue/nested.rs`,
`t/control/begin-prologue-nested.t`).

- **What is lifted.** A statement-form or value-form BEGIN nested in a routine,
  closure, loop, conditional or block is lifted into the prologue. It goes
  just ahead of the top-level statement that contains it. A value-form BEGIN
  at the unit's top level is lifted the same way. The prologue's bound
  (slice 1) extends to the last statement with a lifted effect.
- **Value.** A value-form BEGIN, or a statement-form one that ends its block,
  stores into a unit-level slot (`__begin_value_N`), and the site reads that
  slot, decontainerized (`$slot<>`) so a list value still flattens into an
  array. This uses a slot rather than §2.2's `site_id` / `once_store` pairing:
  the slot is an ordinary unit lexical captured like any other, so it needs no
  second mechanism.
- **Static cells (§2.2).** An inner lexical the body reads gets a unit-level
  cell (`__begin_cell_N`).
  - The lifted body runs in a block that declares the name from the cell and
    copies it back afterwards.
  - The inner declaration moves to its scope's head and starts from the cell,
    marked `__begin_static` so the reorder pass keeps it whole. Its
    initializer stays in place as an assignment.
  - A parameter the body reads is a fresh, unbound declaration. An `our`
    variable is re-declared.
  - The free names come from compiling the body on its own
    (`CompiledCode::free_var_syms`).
- **Nested `constant`.** A nested `constant` whose initializer reads a
  cell-backed lexical is lifted the same way, so it sees the static value
  (`roast/S04-declarations/constant.t` test 27).
- **Deviation from §2.1.3.** An `@`/`%` lexical is copied from its cell on
  each scope entry, where rakudo binds the same object into every frame
  (`sub f { my @a; BEGIN @a.push(1); @a.push(2); say @a }; f(); f()` prints
  `[1 2]` twice, where rakudo prints `[1 2]` then `[1 2 2]`). Every scalar
  case matches rakudo.
- **Not lifted.** These keep their pre-ADR handling (`BeginOnceExpr`, the
  nested reorder rules):
  - a BEGIN whose enclosing inner scopes declare ahead of it a routine, a
    code variable (which can declare an operator), a type, a package or an
    import (a plain routine no longer does, and nor do most types, packages,
    imports and code variables: see the two follow-ups below);
  - a BEGIN in a package body, including a method's;
  - a blockless `BEGIN my %h = ...`, whose `my` declares into the enclosing
    scope;
  - a BEGIN whose body uses a placeholder, which is `X::Placeholder::Block`;
  - a BEGIN that reads a name the unit does not declare (an EVAL's caller
    lexical, for example), or reads a `state`, `constant` or group-declared
    inner lexical;
  - `will begin`.

  Once one BEGIN-time effect is not lifted, no later nested one is, because
  lifting it would run it ahead of an effect that precedes it in the source
  (`roast/S04-declarations/will.t`).

**Slice 2 follow-up — a routine declared ahead of a nested BEGIN, implemented**
(`src/runtime/begin_prologue/nested/routines.rs`,
`t/control/begin-prologue-inner-subs.t`; closes #10329).

- **The gap.** An inner scope that had declared a `sub` ahead of a BEGIN
  blocked the lift, because the prologue runs before that scope is entered and
  the routine does not exist there. `sub f { sub helper { 1 }; BEGIN say "b" }`
  never ran its BEGIN when `f` was not called.
- **The mechanism.** The lifted body gets a copy of each routine it calls. It
  runs in one block per scope it reads from, nested as those scopes are. A
  scope's block declares the copies of the variables the body (or a copied
  routine) reads from that scope, taken from the same static cells as before,
  then the routines declared in that scope, then the body. So a routine closes
  over the same declarations it does in place, and a name shadows as it does
  there. The routine's own declaration stays in place, so each frame of the
  scope still gets its own. This is the first alternative of the issue
  (re-declare the routine in the lifted body's block). Giving routines static
  cells was not needed.
- **Which routines.** The compiled body is scanned for the routines it calls by
  bare name and reads as `&name`. Each routine it selects is scanned the same
  way, so the closure is transitive, and its free variables resolve against the
  bindings that preceded its own declaration. A plain `sub` is copyable. A
  `multi`, an `our sub`, an exported routine, an operator or other category
  routine (its syntax is already registered by the parser, which a scan of the
  called names cannot see), and a redeclaring one still block the scope
  ([#10395](https://github.com/tokuhirom/mutsu/issues/10395)).
- **Dynamic access.** `EVAL`, `CALLER::`/`OUTER::`/`MY::`, a pseudo-package
  qualified call (`MY::helper()`) and `::($name)` can name any routine in
  scope. A body that uses one in a scope that declares a routine keeps its
  pre-ADR handling, as before. Lifting it instead would fail at startup, where
  the old handling was silent. So does a body that calls a routine which is
  neither one of the scope's (copied) nor a core one
  (`Interpreter::is_builtin_function`): an imported or unit-level routine may
  evaluate a string where it was called from, and `BEGIN throws-like
  'lightning()', ...` names `lightning` only inside that string
  (`roast/S06-advanced/stub.t`). The scan sees the callee names in the body and
  in every copied routine, qualified ones included. It cannot see through a
  callee, which is why the rule is about what may be called, and stays in force
  only in a scope that declares a routine (a scope with none is unchanged).
- **Still not lifted** at the time: a type, package, import or `my &code`
  declared ahead. See the next follow-up.

**Slice 2 follow-up — a type, package, import or code variable declared ahead
of a nested BEGIN, implemented** (`src/runtime/begin_prologue/nested/decls.rs`,
`t/control/begin-prologue-inner-decls.t`; closes #10394).

- **Imports.** An import (`use Foo`, `need Foo`, `import Foo`) can bring in any
  name, operators included, so the lifted body's block for its scope repeats
  it. The module is loaded once either way and the import binds the same
  exported objects; rakudo performs it at BEGIN time too. A lowercase pragma
  (`use strict`, `use lib`, `no ...`) still blocks the scope: mutsu applies most
  of them as run-time state at their position.
- **Code variables.** A `my &g` is an ordinary lexical: a BEGIN that reads or
  assigns it gets a static cell like any other, and a bare call `g()` reads it
  when it is the innermost `&g`. A code variable with an operator name
  (`my &infix:<x>`) still blocks the scope, since the parser has already
  registered its syntax, which no scan sees.
- **Types and packages.** The references are resolved on the AST rather than
  in the compiled code (the second alternative of the issue). The typed
  visitor of [ADR-0137](0137-typed-ast-visitor-for-analyses.md) reports every name the body,
  its copied routines and its copied variable declarations mention, in every
  position: a type constraint, a parameter's type, a qualified name, source
  text compiled later. It never reports a string literal's content. A
  symbolic lookup or a pseudo-package counts as naming every type.
  - A BEGIN that names none of the scope's types is lifted without them.
  - A BEGIN that names one gets the declaration repeated in its block (the
    first alternative), and so does every type that one names in turn
    (`is Base`). This is done only when the repeat is unobservable: the body
    holds declarations only (attributes, methods, routines, `does`, nested
    pure types), no user trait runs code, no BEGIN of its own sits in it, and it
    names nothing of the inner scopes. A `my` type is stored under its
    declaration site (ADR-0047), so the repeat registers the same type the
    scope declares in place, as each entry of the scope already does: `sub f {
    my class K { }; my $t = BEGIN K; $t === K }` is `True`, as on rakudo.
  - As for routines, a body that reaches a name dynamically (`EVAL`,
    `::($name)`) or calls a routine it does not know is not lifted from a scope
    that declares a type. A call that is a coercion to an inner type (`K(...)`)
    or qualified by an inner package (`P::x()`) is known.
- **Still not lifted.**
  - A BEGIN that names a type whose body runs code (`my class K { say 1 }`),
    has a user trait, or reads a lexical of the scope. Repeating it would run
    that code at BEGIN time.
  - A BEGIN that names an inner type and may change a type through its
    metaobject (a metamethod other than a read-only one, `.HOW`, `augment`).
    The scope's in-place declaration registers the type afresh, so a change
    made to the repeat would be lost
    (`t/vm/scope/lexical-class-refines-builtin.t`).
  - A BEGIN that reads an inner variable whose type constraint names an inner
    type (`my K $v`): the variable's static cell is declared at the unit's
    level, where the type does not exist.
  - An operator code variable or a pragma declared ahead (lifted since: see
    the next follow-up), and `class ::($name)`, which has no static name.

  The first non-liftable BEGIN still halts lifting for the rest of the unit
  (`Lifted::halted`), so one of these also keeps the BEGINs after it on the old
  path.

**Slice 2 follow-up — an operator code variable or a pragma declared ahead of a
nested BEGIN, implemented** (`src/runtime/begin_prologue/nested/pragmas.rs`,
`t/control/begin-prologue-inner-pragmas.t`; closes #10472).

- **Operator code variables.** Code uses `my &infix:<x>` through the
  operator's syntax (`1 x 2`), which the parser registered for the rest of the
  scope, or by symbolic lookup (`&::("infix:<x>")`). Neither has to name the
  variable where a scan of the compiled code would see it. So every operator
  code variable of the scopes around a lifted BEGIN gets a static cell and is
  declared in its scope's block, whether or not the body seems to use it (the
  first alternative of the issue). The BEGIN sees the variable's static value,
  and what it stores there is what each entry of the scope starts from, as for
  any other lexical. A `state` one is opaque and still blocks the lift.
- **Pragmas.** The lifted body's block repeats a lexical pragma of its scope,
  as it repeats an import. Whether that is sound depends on how mutsu applies
  the pragma:
  - `strict`, `newline` and `no strict` / `no fatal` set interpreter modes
    that the block's `ImportScope` region saves and restores. `soft`, `nqp`,
    `isms`, `v6`, `oo`, `class`, `experimental`, `customtrait`, `warnings`, and
    `no` of `isms`, `worries`, `precompilation` or `soft`, are no-ops in mutsu.
    They are repeated.
  - `use fatal` also marks a routine compiled after it, and `use variables` /
    `use dynamic-scope` change a variable declaration compiled after it. The
    block puts its pragmas ahead of its copied declarations, so each of these
    is repeated only when the BEGIN copies nothing of its scope that precedes
    it there; otherwise the BEGIN is not lifted.
  - Any other pragma still blocks the scope. `use lib` and `use if` act beyond
    the block (mutsu does not yet apply a nested `use lib` at BEGIN time
    either), `use attributes` is not restored on block exit, and a pragma mutsu
    does not implement (`use worries`, `use trace`) fails at run time, which
    repeating it would move to startup.
- **Still not lifted.** A BEGIN that relies on `no strict` to auto-declare a
  variable, since the undeclared name resolves to nothing the unit declares,
  and the pragmas listed above.

**Slice 3 — `use`, `constant` and conditional `use` implemented**
(`src/runtime/begin_prologue/mod.rs`,
`t/modules/import-export/use-if-begin-time.t`,
`t/modules/import-export/use-constant-begin-time.t`; closes #9919 and #10336).

- **Bound.** The prologue's bound reaches the last top-level BEGIN-time
  effect: a BEGIN, a `constant`, and every `use` / `need` / `import` except a
  positional pragma. So a unit's loads and constants all run in the prologue,
  in source order, ahead of the run-time statements that precede them.
- **Positional pragmas.** A lowercase pragma other than `use lib` and `use if`
  (`use strict`, `no strict`, `use fatal`, `use soft`, ...) is applied by
  mutsu as run-time state at its own position. It stays in the run-time
  remainder and does not extend the bound; moving it would switch the mode on
  for the statements before it. `use lib` and `use if` move, because later
  loads depend on them.
- **Multi-statement declarations.** The mainline takes its prologue before
  flattening its `SyntheticBlock`s, so a desugared declaration (`my ($a, $b) =
  f()`, whose members carry no `__has_initializer` marker) stays whole in the
  run-time remainder instead of losing its initializers. An exported type
  (`class C is export { }`, the declaration plus its `__MUTSU_EXPORT_TYPE__`
  marker) moves whole into the prologue.
- **Block imports over an outer import.** A block's `use` that re-imports a
  name the unit already imported (`use M :t; { use M } t`) used to remove the
  name on block exit. The import scope now remembers the value it shadowed
  and puts it back. The prologue made this common (the unit's `use` now runs
  before the block's), but the bug was independent of it.
- **Conditional `use`.** `use Foo:if(EXPR)` evaluates `EXPR` in the prologue,
  into a unit slot the `use` then reads. An undefined value dies with `Did not
  provide compile-time-value for :if adverb in use statement`, before the
  mainline runs. The run-time guard around `UseModule` stays, but it now runs
  in the prologue.
- **Native types.** A native variable split ahead of a prologue effect starts
  its static half from the native zero. A native type with no known zero (a
  NativeCall `ulong`) keeps its declaration whole.
- **Residue:**
  - A `False` condition still leaves the names the parse-time scan registered
    for the module in place (#10331). Calling one therefore fails at run time
    rather than at compile time. Fixing it needs §2.4's parse feedback.
  - The undefined-condition error is a plain `die` raised in the prologue, not
    a `===SORRY!===` compile error.
  - A `use` nested in a block still loads through GH-8201's `PreloadModule`
    hoist, not the prologue, and `constant`s nested in inner scopes run in
    position unless slice 2 lifts them. Only top-level loads and constants
    are prologue effects.

**Nested type declarations — implemented** (#10494,
`src/compiler/hoist_nested_types.rs`,
`t/oo/role/nested-class-runs-role-body-at-compile-time.t`).

- The compile-time shell of an `our` class or role declared inside code
  (#10470) is a BEGIN-time effect. In a unit with a nested class that
  composes a role, the partition collects each top-level
  statement's nested type declarations into a `Stmt::NestedTypeShells` marker,
  placed in the prologue after that statement's own declaration part, and the
  prologue's bound extends to the last such statement. The compiler emits a
  shell registration for each entry there.
- The shell is the compile-time composition: it runs the composed roles'
  bodies through the composition memo, so a role body bumping a unit lexical
  declared above sees it in its static state, and the in-place registration
  repeated on every entry of the enclosing code runs the body no more. That
  registration gets back the nested-block method captures the shell's run
  filed.
- **Residue:** any other unit (and a run-time chunk, which gets no
  partition) still shells its nested declarations at its head. No shell there
  runs user code, so the place is not observable; it is kept because
  extending the bound over every unit with a nested type exposes partition
  bugs ([#10524](https://github.com/tokuhirom/mutsu/issues/10524)).

**INIT and CHECK in a type, package or routine body — implemented** (#10552,
`src/runtime/begin_prologue/package_phasers.rs`,
`t/modules/init-check-in-package-body.t`).

- **The gap.** The per-level phaser reordering (`runtime/phasers.rs`) stops at
  a class, role or package body, so an `INIT`/`CHECK` there ran when the body
  ran: after the mainline statements ahead of the declaration, and once per
  composition in a role. A routine the prologue takes got only the per-level
  recursion, so an `INIT` in it ran on each call, and never if the routine was
  not called.
- **The mechanism.** Before the partition, each top-level type, package and
  `sub` declaration gives up its statement-form INIT/CHECK phasers (and, in a
  routine, a value-form one that is a declaration's or assignment's whole
  initializer). Each becomes a top-level phaser placed just ahead of the
  declaration, where the unit's reordering puts it among the unit's own INITs
  (source order) and CHECKs (reverse order). This is the issue's
  "unit-level queue": the unit's own INIT/CHECK sequence, reached from any
  depth of a declaration.
- **Lexicals.** A phaser of a class or brace-scoped package body, or of a
  method or `sub` of one, runs inside a `Stmt::PackageRuntimeBody` of each
  package it is nested in. That re-enters the package and binds the body's `my`
  lexicals from the package's static store, the same store slice 1's split
  and the body's methods use, so it needs no cell of its own. The
  declaration becomes a BEGIN-time effect (the prologue's bound extends to
  it), so the package is composed before any INIT runs and the lexicals hold
  their static value, as on rakudo (`class C { my $x = 3; INIT say $x }` says
  `(Any)`). `$?CLASS` and `$?PACKAGE` are the package's.
- A value-form phaser stores into a unit-level slot (`__init_value_N`) that
  heads the prologue; the site reads the slot.
- **Not moved.** These keep the per-level handling:
  - a phaser of a routine that reads one of the routine's own names (a
    parameter, `self`, a lexical or routine it declares), an attribute, or
    anything through `EVAL` or a symbolic lookup; or, in a package body, one
    of the body's `our`, `state`, dynamic or code variables, which the store
    does not hold;
  - a role-body phaser that reads a name the role body declares, a role
    parameter or a `$?` variable (a role body has no store to re-enter);
  - a phaser nested in a block, loop or closure inside such a body, and a
    declaration that is not at the unit's top level.
- **Prologue routines.** The phasers the per-level lift finds in a statement
  the prologue took (a nested or value-form one the move above leaves) are now
  lifted to the remainder's level as well, their slots declared at the head
  of the prologue.
