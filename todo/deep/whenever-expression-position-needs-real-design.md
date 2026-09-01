# `whenever` in expression position needs real design, not a binding-bug fix

> **Superseded by [ADR-0053](../../docs/adr/0053-do-whenever-produces-a-tap-on-the-stack.md)
> (2026-08-20).** Read the ADR, not this file, before implementing.
>
> The re-investigation confirmed the *symptoms* below but **disproved "Root
> cause 1"**: bare `whenever` in expression position is not a missing parser
> feature — rakudo rejects that source outright (`Word 'whenever' interpreted
> as a listop; please use 'do whenever' to introduce the statement control
> word`), exactly as it rejects `my $x = if 1 { 2 }`. The legal form,
> `do whenever … { … }`, **already parses correctly in mutsu** into the
> `Expr::DoStmt(Stmt::Whenever { … })` shape this file's "what a real fix
> needs" item 1 proposed building. No parser work is required.
>
> "Root cause 2" is confirmed and is the whole defect, and it is worse than
> recorded here: all four (react|supply × bare-`Var`|`MethodCall`) cells answer
> wrongly (`Supply` or `Any` where raku answers `Tap`), the non-react `.tap()`
> arm that *does* insert a real Tap is unreachable from `do whenever` in either
> legal context (verified by breakpoint), and the only working shape is an
> `IO::Socket::Async::Listener` source — the sole reason
> `roast/S32-io/IO-Socket-Async.t`'s `isa-ok $listen-tap, Tap` is green.
> ADR-0053 replaces the name bridge with an ordinary stack value plus a
> subscription identity that makes `Tap.close` work, in three slices.
>
> The "mutsu accepts statement control words in term position where rakudo
> errors" family (`if`, `whenever`, …) is a separate parser-diagnostic
> question; ADR-0053 §4 records it as out of scope.

Reclassified from `todo/tickets/whenever-target-var-binds-wrong-value-in-react.md`
after investigation showed the root cause is a missing language feature
(the parser does not support `whenever` as an expression term at all),
not a narrow VM binding bug — and that the one existing mechanism that
*does* let `whenever` appear in expression position is itself broken even
for its own narrow intended case. This needs real design work across the
parser, AST, compiler, and VM, not a one-shot patch.

## Original repro

```raku
my $s = Supplier.new;
react {
    my $tap = whenever $s.Supply -> $x {
        say "got $x";
    }
    say "tap type: {$tap.WHAT.raku}";
    say "tap raku: {$tap.raku}";
    $s.emit(1);
    whenever Promise.in(0.2) { done }
}
```

Expected: `$tap` is a `Tap` instance. Actual: `$tap` is bound to the
literal string `"whenever"`.

## Root cause 1: the parser does not parse `whenever` as an expression term at all

`--dump-ast` on the repro shows `my $tap = whenever $s.Supply -> $x {...}`
is NOT parsed as one construct. It fragments into four independent
statements:

1. `VarDecl { name: "tap", expr: BareWord("whenever") }` — the RHS
   expression parser has no rule for the `whenever` keyword as a term, so
   it falls back to treating it as a plain bareword/identifier and stops
   there. This is where the literal `"whenever"` string comes from.
2. `Expr(MethodCall { target: Var("s"), name: "Supply" })` — `$s.Supply`
   as an orphaned statement.
3. `Expr(Lambda { param: "x", body: [...] })` — the `-> $x {...}` pointy
   block as ANOTHER orphaned statement.
4. (later) the real `Stmt::Whenever` — but only because the *bare*
   `whenever` statement grammar (`whenever_stmt` in
   `src/parser/stmt/control/react.rs`) independently recognizes `whenever`
   as a STATEMENT keyword; it has no connection to `$tap` at all.

`whenever_stmt` (`src/parser/stmt/control/react.rs`) only ever gets
invoked from statement-level contexts (`react_stmt`'s shorthand, or the
generic statement dispatcher). There is no path that lets an expression
parser (e.g. `VarDecl`'s RHS, or the generic primary-expression parser)
recognize `whenever` as a term and delegate to it.

## Root cause 2: the one existing "whenever in expression position" mechanism is independently broken

There IS a pre-existing, narrower mechanism for `do { whenever $s {...} }`
specifically — `Stmt::Whenever`'s arm in
`src/compiler/expr_block.rs:686-698`, gated by `self.whenever_bind_target`
(set only while compiling a `do{}` block's tail statement) and a
`target_var_idx` field already on the `OpCode::WheneverScope` opcode
(`src/opcode.rs:1916`, wired in `src/compiler/stmt.rs:4038-4057`).

Reading the actual logic (both the compiler arm and the opcode wiring)
shows this mechanism is narrower and buggier than its own design intent:

- `target_var_idx` is only ever set when `supply` (the whenever'd
  expression) is LITERALLY `Expr::Var(name)` — a bare variable read, e.g.
  `whenever $s {...}`. Anything else (`$s.Supply`, a method chain, a
  computed expression) leaves `target_var_idx` as `None`, and
  `expr_block.rs`'s fallback is `OpCode::LoadNil` — so
  `do { whenever $s.Supply -> $x {...} }` already answers `Any` even
  before this ticket's original repro's parse-level fragmentation is
  considered.
- Even for its OWN designed-for shape (`whenever` on a bare Supply
  variable), it works by **clobbering the SOURCE variable itself** —
  `target_var_idx` names `$s`, and the runtime is expected to rebind `$s`
  (not a fresh variable) to the resulting Tap, which `expr_block.rs` then
  reads back out as the `do{}` block's value. This is confirmed broken by
  direct test: `my $tap = do { whenever $s -> $x {...} }` (with `$s` a
  bare `Supply` value) answers `$tap.WHAT.raku` as `"Supply"`, not
  `"Tap"` — so even the value the runtime leaves under `env[$s]` after the
  `whenever` fires is not actually the live Tap.
- The clobbering design is also semantically WRONG for the general case
  even if it worked: `whenever_bind_target`'s own comment
  (`compiler/stmt.rs:4038-4043`) explicitly says a **bare** `whenever $s
  {...}` statement must NOT clobber `$s` with its Tap (a nested `whenever`
  re-tapping the same Supply on a later iteration needs to keep seeing
  the Supply, not a Tap) — so this mechanism can only ever be safe for the
  narrow `do{}`-wrapped case, and even there it conflates "the variable
  holding the source Supply" with "the variable that should hold the
  resulting Tap," which are two different things once `my $tap = whenever
  $s {...}` (a NEW distinct variable) is what's actually wanted.

## What a real fix needs

This is not a one-line binding-bug fix; it needs:

1. **Parser**: recognize `whenever <supply-expr> [-> <param>] { <body> }`
   as a valid primary expression term (not just a statement), likely
   wrapping the result the same way `do {}` wraps a statement-as-expression
   (`Expr::DoStmt(Box<Stmt>)`) — probably via a dedicated `Expr` variant
   or by routing through `Expr::DoStmt(Box::new(Stmt::Whenever {...}))`
   so `my $tap = whenever ... {...}` and `$tap = whenever ... {...}`
   (assignment, not just declaration) both parse as one construct instead
   of fragmenting.
2. **AST/compiler**: a `target_var_idx`-equivalent that does NOT depend on
   `supply` being a bare `Expr::Var` — every expression-position
   `whenever` needs its own fresh, non-clobbering binding slot for the
   resulting Tap, regardless of what kind of expression produced the
   Supply being tapped.
3. **Runtime** (`vm_scope_ops.rs::exec_whenever_scope_op`,
   `runtime/subtest.rs::run_whenever_with_value`): confirm/fix that the
   value actually stored under the target binding is the genuine `Tap`
   object `.act()` (or the equivalent live-subscription path) returns —
   not the original Supply, not `Nil`. The ticket's own investigation
   notes the generic (non-`IO::Socket::Async::Listener`) case in
   `run_whenever_with_value`'s "in react mode" branch does
   `self.env.insert(name.to_string(), supply_val)` — binding the SOURCE
   Supply, not a Tap — which is consistent with what direct testing shows
   here.
4. Decide what happens to the existing narrow `do { whenever $s {...} }`
   clobbering mechanism: subsume it into the general fix (so it stops
   clobbering `$s` and instead gets its own fresh slot too), or keep it as
   a separate special case — needs a decision, not an assumption.

## Impact

Any code that needs to hold onto a live `whenever`'s `Tap` handle from
inside a `react`/`supply` block (e.g. to `.close()` it early, or check
`.closed`) is currently getting garbage instead — either a stray bareword
string (the direct `my $tap = whenever ...` shape) or `Any`/the original
Supply (the `do { whenever ... }` shape). This is a real, general,
likely high-impact gap for any Cro::HTTP-style or hand-written
Supply/Channel consumer that manages its own tap lifecycle.

## Repro

```sh
cargo build
timeout 15 ./target/debug/mutsu -e '
my $s = Supplier.new;
react {
    my $tap = whenever $s.Supply -> $x { say "got $x" };
    say $tap.WHAT.raku;
    say $tap.raku;
    $s.emit(1);
    whenever Promise.in(0.2) { done }
}'
# prints "Str" / "\"whenever\"" instead of "Tap" / a Tap .raku rendering

timeout 15 ./target/debug/mutsu -e '
my $s = Supplier.new.Supply;
react {
    my $tap = do { whenever $s -> $x { say "got $x" } };
    say $tap.WHAT.raku;
    whenever Promise.in(0.2) { done }
}'
# prints "Supply", not "Tap" -- even the narrow do{}-wrapped case is broken
```

## Re-verified 2026-09-01 (TRIAGE regeneration): the symptom moved

Both legal shapes (`my $tap = do whenever ...` and `do { whenever ... }`) now
answer `Tap` for `.WHAT` and deliver `got 1` — the `Str "whenever"` /
`Supply` / `Any` symptoms above are gone. What is still wrong is the
**subscription identity** half: after `$s.emit(1); $tap.close; $s.emit(2)`,
raku prints `got 1` then `done`, but mutsu prints only `done` — closing the
Tap retroactively drops the value emitted *before* the close. ADR-0053's
header still says "implementation not started", which is stale relative to
the `.WHAT` result; whoever picks this up should first reconcile the ADR with
whatever landed, then implement the identity slice.
