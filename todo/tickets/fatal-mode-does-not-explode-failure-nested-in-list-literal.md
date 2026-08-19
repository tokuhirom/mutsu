# `use fatal` does not immediately explode a Failure constructed inside a nested list/array-literal expression

Found while fixing
`todo/tickets/bare-failure-sink-is-consumption-time-not-creation-time.md`
(now resolved — see `news/2026-08/failure-fatal-mode-creation-time.md`). That
fix made a bare-variable sink (`$f;`) correctly exempt from forcing an
unhandled `Failure`, which is what real Raku's "Useless use of ... in sink
context" optimization does; construction-time explosion under `use fatal`
continues to rely on the various `self.fatal_mode` checks already present at
each assignment opcode (`vm_var_assign_local.rs`,
`vm_var_assign_set_local.rs`, `OpCode::SinkPopAssign`, the typed-assignment
arm in `vm_exec_dispatch.rs`, `vm_closure_dispatch.rs`'s `captured_fatal_mode`
replay).

Those construction-time checks all key off a **simple assignment RHS**
(`my $x = EXPR;`, `$x = EXPR;`). They do not cover a `Failure` produced
*inside* a larger expression that is not itself the direct RHS of an
assignment to a single scalar — e.g. one element of a list/array literal:

```raku
use fatal;
my @a = (1, "a".Int, 3);
say "reached";
```

`raku`: throws `Cannot convert string to number: ...` immediately while
building `@a` (never reaches `say`). `mutsu` (current, both native and
`MUTSU_REAL_TEST=1`): builds `@a` with the coercion's `Failure` as an element,
prints `reached`, and only would explode later if/when that element is
actually read (numeric/string/boolean context, an explicit sink of that
specific element, etc.) — deferring a decision Raku makes eagerly at
construction.

Verified via `git stash` that this reproduces identically on `main` before
the sibling ticket's fix landed — it is a pre-existing, independent gap, not
introduced by that change.

## Why this is bigger than a one-line fix

The mutsu architecture builds Failure values as ordinary, fatal-mode-agnostic
`Value::make_instance(Symbol::intern("Failure"), ...)` calls at roughly 30
call sites (`grep -rn 'make_instance(Symbol::intern("Failure")'`), most of
which are free functions / associated functions with no `&Interpreter`
access at all (e.g. `RuntimeError`'s constructors in
`src/value/error_construct.rs`, `builtins/methods_0arg/dispatch_core_coerce.rs`'s
native coercion methods). None of them know the ambient `fatal_mode`.
Matching Raku's real "explode immediately at construction when fatal is
active" semantics fully — not just for the direct-assignment shapes the
current ad hoc checks happen to cover — needs either:

- Threading `fatal_mode: bool` down to every Failure-construction call site
  (a real, cascading signature change through many call chains that
  currently have no VM access at all), or
- A more surgical approach: after a native method / builtin call returns a
  freshly-constructed value anywhere (not just the handful of ~7
  assignment-shaped opcodes that already check `self.fatal_mode`), check
  whether it is an unhandled Failure and `self.fatal_mode` is active, and
  explode immediately instead of letting it become a stored value at all —
  including when that value ends up nested inside a `MakeArray`/`MakeHash`/
  `MakeRealArray` composite the VM constructs right after.

Either direction is a genuine, non-trivial architectural change (more sites
than the sibling ticket's sink-side fix), so it is out of scope for a
same-day slice. Repro and root cause are recorded here so the next session
does not have to re-diagnose it.

## Minimal repro

```raku
use fatal;
my @a = (1, "a".Int, 3);
say "reached";
```

`raku`: throws at the `my @a = ...` line. `mutsu`: prints `reached`.
