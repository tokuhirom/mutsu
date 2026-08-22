# `do when COND { BLOCK }` used as an expression gives the wrong value / crashes

Discovered via the doc-diff harness on `raku-doc/doc/Language/control.rakudoc` (around line
526/537).

## Repro

```
$_ = True;
my $a;
{ $a = do when .so { "foo" } }
say $a;
```

- raku: `foo`
- mutsu: crashes with a bare `Runtime error:` (no useful message)

With the topic not matching:

```
$_ = False;
my $a;
{ $a = do when .so { "foo" } }
say $a;
```

- raku: `(Any)` (the `do when` expression evaluates to `Any` when the `when` does not match)
- mutsu: currently gives an inconsistent result (does not crash but the value is wrong — see
  investigation notes below); needs re-verification once the crash is fixed.

## Root cause (unconfirmed, needs a debugger session)

`do EXPR` normally just evaluates `EXPR` and returns its value. When `EXPR` is a bare `when`
statement (`do when COND { BLOCK }`), Raku treats this as an expression form: if `COND` matches
against `$_`, evaluate `BLOCK` and use its value; otherwise the whole `do when` evaluates to
`Any`. mutsu's `when` is implemented as a control-flow statement (see `vm_control_ops.rs`) that
presumably assumes it is always used in statement position inside a `given`/loop body, and
doesn't have a value-producing path when wrapped in `do`.

## Affected files (starting point)

- `src/compiler/expr.rs` / `src/compiler/stmt.rs` — wherever `do BLOCK`/`do STATEMENT` is
  compiled, to see how it handles a `when` operand
- `src/vm/vm_control_ops.rs` — `when` execution

## Suggested next step

Reproduce under `rust-gdb` per the debugging guidelines to find exactly which opcode sequence
`do when` compiles to and where the crash originates, then decide whether `do when` needs a
dedicated compiler path that captures the block's value (or `Any` on no-match) instead of
relying on the statement-position `when` control flow.
