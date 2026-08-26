# A `$_` pointy parameter on a conditional leaks into the enclosing scope

Found while implementing `unless COND -> $x { }`
(`news/2026-08/unless-pointy-block-param.md`), 2026-08-26, against raku
v2026.06.

## Repro

```raku
$_ = 1;
if 5 -> $_ { say $_ };
say $_;
```

- raku: `5` then `1`
- mutsu: `5` then `5`

Same for the `else` form, and (since it now shares the same lowering) for
`unless`:

```
$ mutsu -e '$_ = 1; if 0 { 9 } else -> $_ { say $_ }; say $_;'   # 0 / 0   (raku: 0 / 1)
$ mutsu -e '$_ = 1; unless 0 -> $_ { say $_ }; say $_;'          # 0 / 0   (raku: 0 / 1)
```

A non-topic parameter does not leak — `if 5 -> $x { }` leaves no `$x` behind —
so this is specific to `$_`.

## Root cause direction

`lower_if_clause_binding` / `lower_else_binding`
(`src/parser/stmt/control/conditionals.rs`) lower a single simple pointy
parameter to a `binding_var` on the `Stmt::If` (then-branch) or to a
`Stmt::VarDecl` prepended to the else body. For an ordinary name that declares a
block-scoped lexical; for `$_` the declaration lands on the interpreter's topic
slot (env key `"_"`, no sigil) and is never restored on block exit. The
constructs that *do* scope the topic correctly (`given`, `for`) go through the
dedicated topic opcodes (`SetTopic` / `RestoreTopic`) instead of a plain
`VarDecl`.

## Why it was not fixed with the `unless` work

It is pre-existing and independent of `unless` — it reproduces identically on
`if`, which has accepted the pointy form for a long time — so fixing it is a
change to conditional topic scoping in general, not to the `unless` arm. Topic
writeback/scoping has a history of being a rabbit hole in this codebase, so it
wants its own slice with its own regression sweep rather than being smuggled
into a parser fix.

## Suggested approach

Route a `$_` conditional binding through the same fresh-topic scope `given`
uses (the `SetTopic`/`RestoreTopic` pair) rather than declaring it as an
ordinary lexical, and check `orwith`, which already lowers its own body through
`given` for exactly this reason (see the comment in `parse_elsif_chain`'s
`orwith` arm — "via `given` so the fresh topic scope is established by the
`given` opcode rather than a plain `$_ = <cond>` assignment").

## Affected files

- `src/parser/stmt/control/conditionals.rs` — `lower_if_clause_binding`,
  `lower_else_binding`
- the compiler's `compile_if_binding_decl` (the `binding_var` consumer)
