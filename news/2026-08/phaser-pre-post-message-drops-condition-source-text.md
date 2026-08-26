# `X::Phaser::PrePost` quotes the failed `PRE`/`POST` condition's source text

Found by the doc-diff harness (`docs/doc-diff-backlog.md`,
`Type/X/Phaser/PrePost.rakudoc:15`).

```raku
sub f($x) { PRE { $x ~~ Int } };
f "foo";
CATCH { default { put .^name, ': ', .Str } };
```

- raku, and now mutsu: `X::Phaser::PrePost: Precondition '{ $x ~~ Int }' failed`
- mutsu before: `X::Phaser::PrePost: Precondition '' failed`

## Root cause

The exception type, the `phaser` attribute and the message *shape* were all
already right; only the condition text was empty. mutsu reconstructed it by
**deparsing the AST** (`Compiler::deparse_phaser_condition`), which covered only
bare literals and variables — so the statement form `PRE 0` reported `0` and
every block form reported nothing at all.

Deparsing cannot work here, because raku's `.condition` is the **verbatim source
slice** of the phaser's argument, braces and line breaks and indentation
included:

```
X::Phaser::PrePost: Precondition '{
        $x ~~ Int
        && $x > 3
    }' failed
```

## Fix

The slice is now taken in the parser, where the source is still in hand:
`phaser_stmt` records the text consumed between the phaser keyword and the end
of its argument, and `Stmt::Phaser` carries it as a new `condition:
Option<Symbol>` field (`None` for every kind but `PRE`/`POST`, which have no
condition). The compiler interns it as the `CheckPhaser` operand instead of
deparsing, and `deparse_phaser_condition` is gone.

Two further details fell out of checking against real `raku`:

* **The message trims, `.condition` does not.** `PRE 0` inside `sub a { PRE 0 }`
  has `.condition eq "0 "` — the slice runs to the enclosing `}` — while the
  message is `Precondition '0' failed`. `phaser_prepost_error` now trims for the
  message only. (This is what the two existing pins
  `t/phaser-prepost.t` and `t/eval-type-decl-and-phaser-message.t` were
  asserting with `/:s Precondition .0. failed/`, and they caught the difference.)

* **The mainline path was dropping the node.** `split_block_phasers` reduced
  every phaser to a bare `Stmt::Block` of its body, which threw the captured
  text away before the compiler saw it. `PRE`/`POST` now keep their original
  `Stmt::Phaser` node; the new `phaser_body_and_condition` helper unwraps them
  for the three legacy `eval_block_value` carriers, which had been passing a
  hard-coded `""` and now pass the real text too.

## Pin

`t/exception-rendering-and-phasers.t` covers the block form, the `POST` form and
a multi-line condition, byte-for-byte against real `raku`.

## Files

- `src/ast.rs` — `Stmt::Phaser::condition`
- `src/parser/stmt/simple/control_stmts.rs` — the source slice
- `src/compiler/stmt.rs` — `phaser_condition_idx` reads the slice; the deparser
  is deleted
- `src/runtime/mod.rs` — `phaser_prepost_error` trims for the message
- `src/runtime/resolution_call_sub.rs` — `phaser_body_and_condition`,
  `split_block_phasers` keeps the PRE/POST node
- `src/runtime/run.rs`, `src/runtime/resolution_eval.rs` — the legacy carriers
