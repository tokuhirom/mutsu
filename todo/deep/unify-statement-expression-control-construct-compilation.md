# Statement/expression compilation of control constructs is duplicated, not shared

## Root cause

Raku control constructs (`do`/`if`/`for`/`while`/`loop`) can appear in two
positions: as a statement (`if $x { ... }`) or as an expression that produces
a value (`my $y = do if $x { ... }`, or any control construct used in
expression position). mutsu compiles these two positions through two
independent code paths instead of one value-returning pass:

- **Statement position**: `compile_stmt()` in `src/compiler/stmt.rs` (4451
  lines) — the `Stmt::If`, `Stmt::While`, `Stmt::For`, `Stmt::Loop` arms.
- **Expression position**: `src/compiler/helpers_do_expr.rs` (609 lines as of
  2026-08-21, up from 476 lines a revision ago — it is actively growing, not
  stable) — six `compile_do_*` entry points that re-implement the same
  control-flow compilation, plus value-collection on top:
  `compile_do_block_expr`, `compile_do_block_expr_scoped`,
  `compile_do_if_expr_bound`, `compile_do_for_expr`, `compile_do_while_expr`,
  `compile_do_loop_expr`.

The two paths do not share logic; they are two independent implementations of
the same control-flow lowering that happen to diverge in one respect (does
the construct leave a value on the stack). Every bug fix or new semantic (loop
labels, topic binding, writeback, threading) has to be applied twice, and the
two paths can (and historically have) drifted.

### Concrete duplication: `ForLoopSpec` construction

The clearest evidence is `ForLoopSpec` (`src/opcode.rs:194`), the struct that
carries a compiled `for`-loop's parameters to the `OpCode::ForLoop`
instruction. It now has **27 fields** (up from 21 a revision ago — the
struct itself is growing, which means both call sites are growing with it).
It is constructed in exactly two places, once per compilation path:

- `src/compiler/stmt.rs:2507` — inside `compile_stmt()`'s `Stmt::For` arm.
- `src/compiler/helpers_do_expr.rs:382` — inside `compile_do_for_expr()`.

Every time a `for`-loop feature is added (a new writeback mode, a new topic
rule, a new threading flag), both `ForLoopSpec` literals need the same new
field populated correctly, by hand, with no shared helper enforcing parity.

## Affected files

- `src/compiler/stmt.rs` — statement-position control-construct compilation
  (`compile_stmt()`, `Stmt::If`/`Stmt::While`/`Stmt::For`/`Stmt::Loop` arms).
- `src/compiler/helpers_do_expr.rs` — expression-position duplicate
  (`compile_do_block_expr`, `compile_do_block_expr_scoped`,
  `compile_do_if_expr_bound`, `compile_do_for_expr`, `compile_do_while_expr`,
  `compile_do_loop_expr`).
- `src/opcode.rs:194` (`ForLoopSpec`) — the shared payload struct that both
  paths populate independently.

## Why it is large

This is not a small mechanical rename — it is a compiler-architecture change:

- The fix is "one value-returning pass": `compile_stmt()`'s control-construct
  arms need to grow the ability to optionally leave a value on the stack
  (mirroring what `compile_do_*` does today), and the `compile_do_*` family in
  `helpers_do_expr.rs` needs to be deleted in favor of calling the unified
  statement-compilation path with a "produce a value" flag. That touches the
  highest-traffic, highest-line-count parts of the compiler
  (`stmt.rs` is already the largest file in `src/compiler/`).
- `ForLoopSpec`'s 27 fields all need to keep meaning the same thing regardless
  of which call site builds it, across loop labels, topic binding
  (`topic_local`), writeback (`is_rw`/`do_writeback`/`rw_param_names`),
  threading (`race`/`hyper for`), and collection semantics (`collect`) — each
  of which was added incrementally to one or both sites over many PRs, so the
  unification has to audit each field's behavior in both existing call sites
  before merging them.
- Because both paths are reachable from a huge amount of roast-whitelisted
  code (any `if`/`for`/`while`/`loop` anywhere, in either position), a
  behavioral regression from imperfectly merging the two paths would be
  extremely broad — this needs careful incremental validation (`make test` +
  a full `make roast` run), not a single fast PR.
- The duplication has been getting *worse*, not better, across ANALYSIS.md
  revisions (`helpers_do_expr.rs` 476 → 609 lines, `ForLoopSpec` 21 → 27
  fields), because every new control-flow feature has been landing in both
  places independently rather than the unification happening first.

## Repro / evidence

Not a behavioral bug — there is no single failing test. The evidence is
structural duplication, verifiable by inspection:

```
grep -n "fn compile_do" src/compiler/helpers_do_expr.rs
grep -n "ForLoopSpec {" src/compiler/*.rs
```

The second command shows exactly two construction sites
(`src/compiler/stmt.rs:2507` and `src/compiler/helpers_do_expr.rs:382`) for
what should be one shared code path.

## Fix direction

One value-returning compilation pass, per `ANALYSIS.md` §3.1: extend
`compile_stmt()`'s control-construct arms to optionally leave a value on the
stack, then have `helpers_do_expr.rs`'s `compile_do_*` entry points become
thin callers into that unified path instead of independent
re-implementations. Delete `helpers_do_expr.rs`'s duplicated control-flow
logic once the unified path covers its cases; only `ForLoopSpec` (or its
successor) construction should remain, and only once.

See also `ANALYSIS.md` §3.1 and the §7 roadmap table (item 3, "Unify
statement/expression compilation of control constructs") for how this ranks
against other current-priority work.
