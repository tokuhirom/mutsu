# `done VALUE` inside a `supply {}` block drops the value instead of emitting it

Found by the doc-diff harness (`docs/doc-diff-backlog.md`,
`Type/independent-routines.rakudoc:1497`).

## Repro

```raku
my $supply = supply {
    for 1 .. 3 {
        emit($_);
    }
    done 42;  # same as: emit 42; done
}
$supply.tap: -> $v { say "Val: $v" }, done => { say "No more" }
```

- `raku`: `Val: 1` / `Val: 2` / `Val: 3` / `Val: 42` / `No more` — `done 42` emits `42`
  before signaling completion, per the doc's own comment ("same as: emit 42; done").
- `mutsu` (`target/debug/mutsu`): `Val: 1` / `Val: 2` / `Val: 3` / `No more` — the `42`
  is silently dropped; only the bare `done` signal fires.

## Root cause

`src/parser/stmt/simple/control_stmts.rs` (~line 557-571) parses a bare `done`
identifier directly into `Stmt::ReactDone`, with a comment stating `"done() is the
explicit call form (it takes no payload)"`. That's only true for the parenthesized
call form (`done()`); a bareword `done` followed by a value expression
(`done 42;`, no parens) is valid raku syntax and is sugar for `emit 42; done` (per the
doc). The current parsing doesn't consume a following non-empty, non-modifier
expression as an emit-value before treating the statement as `Stmt::ReactDone` — it
either needs a new AST shape (`Stmt::ReactDone` with an optional value) or should
desugar to two statements (`emit VALUE` then bare `done`) at parse time.

## Affected files (starting point)

- `src/parser/stmt/simple/control_stmts.rs` (~line 551-571) — the `done` keyword
  parsing.
- `src/compiler/expr.rs` (~line 181, `Expr::BareWord(name) if name == "done"`) and
  wherever `Stmt::ReactDone` is compiled — needs to thread an optional value through
  to an `emit` before the completion signal.
