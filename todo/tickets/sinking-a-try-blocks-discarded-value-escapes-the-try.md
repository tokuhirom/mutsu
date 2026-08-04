# Sinking a `try` block's discarded value throws *outside* the `try`

`roast/integration/advent2009-day20.t` aborts after 11 of its 21 assertions under
`MUTSU_REAL_TEST=1`:

```
# You planned 21 tests, but ran 11
Stub code executed
  in sub eval_exception at .../Test.rakumod line 1
  in sub eval-lives-ok at .../Test.rakumod line 593
```

The assertion is `eval-lives-ok 'map -> $x, $y { ... }, 1..6'` (line 28), and
`Test.rakumod`'s helper is

```raku
sub eval_exception($code) {
    try {
        EVAL ($code);
    }
    $!;
}
```

`raku` runs the same file and the same module and passes. mutsu does not merely
report a failed assertion — the exception escapes `eval_exception` entirely and
kills the file, which is why it costs ten assertions.

## What was measured

Every row below is `raku` vs mutsu on the same one-liner (all under
`use MONKEY-SEE-NO-EVAL`). "throws" means an *uncaught* `Stub code executed`.

| snippet | raku | mutsu |
| --- | --- | --- |
| `sub ee($c) { try { EVAL ($c); }; $! }` on `map -> $x,$y { ... }, 1..6` | no throw, `$!` undefined | **throws** |
| `sub ee() { my $r = try { EVAL (…); }; $! }` (value captured) | no throw | no throw |
| `sub ee() { try { map -> $x,$y { ... }, 1..6; }; $! }` (no EVAL) | throws | throws |
| `try { EVAL (…); }; say "made it"` at unit scope | throws | throws |
| `my $s = EVAL (…); $s.sink` | throws | — |
| `my $s = map -> $x,$y { ... }, 1..6; $s.elems` | throws | — |

So the divergence is exactly one cell: a `try` block whose **last statement is a
call** and whose value is discarded. `raku` does not sink that value at all;
mutsu sinks it, the sink reifies the `Seq`, the stub block runs and throws — and
the throw is reported *outside* the `try`, at the enclosing routine or unit.

Two independent things are wrong, and they should be judged separately:

1. **The sink happens outside the `try`'s protection.** Whatever the correct
   sink point is, an exception raised while discarding a `try` block's own value
   is currently uncatchable by that `try`. Note this alone is not enough to fix
   the roast file: it would turn the abort into a *failed* `eval-lives-ok`
   instead of a passing one.
2. **The value should not be sunk here at all.** That is what makes `raku` pass.
   Note the third row: `raku` *does* sink and *does* throw when the try block's
   last statement is the `map` itself, so the rule is not "a `try` block's value
   is never sunk". Rakudo's sink-context propagation is static, and a call's
   runtime result is evidently not covered by it the way a `map` statement is.
   Pin down that rule before changing mutsu's, or the third row regresses.

## Where to look

mutsu's statement-level sink for a block-valued statement is emitted in
`src/compiler/stmt.rs` (`SinkPop` and friends); the `try` compilation is
`compile_try` in the same file (see
`news/2026-08/block-local-routine-scope.md` for the `TryCatch { traps }`
distinction between a real `try` and the implicit one). The reify-on-sink of a
`Seq` is `force_lazy_list_vm` (`src/vm/vm_helpers_lazy.rs`).

Related: `todo/deep/deferred-seq-materialization-destroys-the-original.md` is the
other place in this campaign where the real module's strictness meets mutsu's
eagerness, but it is a different mechanism (a `.defined` probe, not a sink).
