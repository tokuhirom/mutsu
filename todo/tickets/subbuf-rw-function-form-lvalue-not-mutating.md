# `subbuf-rw($buf, from, len) = value` (function-call form) silently doesn't mutate

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Buf.rakudoc:84`).

## Repro

```raku
my Buf $b .= new(1,2,3);
subbuf-rw($b,2,1) = Buf.new(42);
say $b.raku;
```

- raku: `Buf.new(1,2,42)`
- mutsu: `Buf.new(1,2,3)` (exit 0, no error — the assignment is silently a no-op)

## Isolating the root cause

The **method-call form** already works correctly:

```raku
my Buf $b .= new(1,2,3);
$b.subbuf-rw(2,1) = Buf.new(42);
say $b.raku;   # mutsu: Buf.new(1,2,42)  -- correct
```

So only the bare **function-call form** (`subbuf-rw($buf, from, len) = value`, no leading `.`)
is broken, even though mutsu does have dedicated handling for it: `src/runtime/
builtins_lvalue.rs` (~line 451-480) special-cases `name == "subbuf-rw"` in an lvalue-assignment
context, and tries to resolve the target variable by scanning `self.env` for an entry whose
value is `values_identical` to `call_args[0]`, then delegates to
`assign_method_lvalue_with_values(target_var, target, "subbuf-rw", ...)`. That delegation
appears to silently no-op when it can't establish where to write back — worth checking whether
the identity search actually finds `$b` (it should, since `$b` is a plain top-level lexical)
or whether the write-back happens but doesn't reach the actual `env` slot/local slot mutsu's
dual `locals`/`env` store needs updated. Compare against the working `substr-rw` function-form
sibling case (same file, lines ~420-449) to see whether it has the identical bug or genuinely
differs.

## Affected files

- `src/runtime/builtins_lvalue.rs` (~line 451-480) — the `subbuf-rw`-as-function lvalue special
  case.
- `src/runtime/methods_mut_substr_buf.rs` — `assign_subbuf_rw`, the actual mutation logic
  reused by both the method-form (working) and function-form (broken) call sites; confirm it's
  being invoked at all for the function form, and with the right target reference.
