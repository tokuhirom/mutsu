# `Code.line` / `Code.file` report a routine's declaration site

`Code` and its subtypes (`Sub`, `Method`, `Submethod`, `Block`, `Regex`) carry
two reflection accessors that name where the routine was written: `.line` and
`.file`. mutsu answered them only for *closures* — an anonymous `sub`, a bare
block, a pointy block — because `SubData::source_line`/`source_file` were filled
in exclusively by the closure-creation opcodes. A **named sub** answered `Nil`
for both, and a **method** obtained via `.^lookup` / `.^find_method` /
`.^methods` raised `X::Method::NotFound` outright:

```
$ target/debug/mutsu -e 'sub f {}; say &f.line; say &f.file'
Nil
Nil
$ target/debug/mutsu -e 'class Food { method eat {} }; say Food.^lookup("eat").line'
No such method 'line' for invocant of type 'Method'
```

The finding came from the doc-diff harness (`Type/Code.rakudoc:166,175,195`).

## The design: one location, carried on the routine's own compiled body

The location is not stored in a new side channel. Every declared routine already
owns a `CompiledFunction`, and that object already had both halves of the slot:
`CompiledCode::source_line` (which the closure paths read) and
`CompiledFunction::source_file` (which `registration_sub::adapt_compiled_to_def`
fills from `FunctionDef::source_file`, and which backtrace frame attribution
reads as `def_file`). They were simply never populated for routine bodies.

Three small changes make the whole feature fall out of that one slot:

1. `Compiler::compile_sub_body` stamps `code.source_line` with the *enclosing*
   compiler's `last_source_line` — the line of the `sub` keyword, i.e. exactly
   the line it already seeds the body's prologue with via `set_emit_line`.
   (`sub_compiler`'s own `last_source_line` is wrong here: it has advanced
   through the body by then.)
2. `Compiler::compile_method_body` does the same for a method. Its nested
   `method_compiler` is a bare `Compiler::new()` with no line history, and a
   method body's first `SetLine` is its first *statement's* line, not the
   declarator's — so `compile_method_body_keys` now walks the type body in source
   order, tracking `Stmt::SetLine`, and hands each method its declaration line.
3. `Value::make_sub_for_routine` reads both fields back off the
   `compiled_routine` it is already given. That single constructor backs all ten
   sites that materialize a code object for a declared routine (`&f`,
   `.candidates`, `.cando`, `nextsame` candidates, operator fallbacks, `.wrap`
   targets), so they all gained the location at once, with no signature change.

For a `Method`, `make_method_object_with_owner_ex` publishes `line`/`file` as
instance attributes next to the `name`/`rw`/`readonly` it already publishes,
reading the line from the `MethodDef`'s installed `compiled_code` and the file
from the `MethodDef::source_file` registration already recorded.

A **multi dispatcher** (`&mm` for a `multi sub mm`) is built by name and has no
compiled body of its own. It answers from its first candidate — matching Rakudo,
where `&mm.line == &mm.candidates[0].line` — via a new
`Interpreter::routine_decl_location`, shared with the by-name `Routine` shape.

Since this is the same slot backtrace frames read as `def_file`, the two open
`Backtrace::Frame` tickets can consume it rather than growing a parallel channel.

## What a routine with no Raku source reports

Rakudo answers `&infix:<+>.file` with `SETTING::src/core.c/Numeric.rakumod` and
`.line` with a line number inside that setting file. mutsu has no Raku setting:
its operators and builtins are Rust functions with no source location at all.
Rather than synthesize a path that looks real, **a native routine reports `Nil`
for both** — the honest "unknown". The important part of the fix there is that
`&infix:<+>.line` no longer *raises*; `Code` promises the accessor exists, and it
now does for every `Code` shape.

The same rule covers a `Regex` code object (a `/.../` literal, or a grammar
`token`/`rule` reached through `.^lookup`): they answer `Nil` instead of raising.
mutsu genuinely does not record a declaration line for either — a regex literal
is a constant in the pool, and `Registry::token_defs` keeps the declaring file
but no line. That remaining gap is tracked in
`todo/tickets/code-line-file-on-regex-and-grammar-token.md`.

## Semantics established against `raku`

Probed on the reference implementation before implementing, and now matched:

- `.line` is the **declarator keyword's** line, not the signature's or the
  block's: a `sub f(\n $x\n)\n{\n ...\n}` reports the `sub` line.
- `.file` is the path **as invoked**, not canonicalized: `raku tmp/x.raku` gives
  `tmp/x.raku`. Note this deliberately does *not* agree with `$?FILE` for the
  main compilation unit — Rakudo's `$?FILE` there is absolute while `.file` is
  relative (verified: `$?FILE eq &s.file` is `False` in raku for a script run
  through a relative path), so the `$?FILE` canonicalization recorded in
  `news/2026-08/dollar-question-file-relative-not-absolute.md` is untouched. They
  do agree inside a `use`d module.
- `.^lookup` on an **inherited** method reports the *declaring* class's file and
  line, not the looked-up class's. A role method keeps the role's location after
  composition.
- A `Code` **type object** raises rather than answering (`Code.line` is an
  invocant-concreteness error in Rakudo); mutsu also raises, so `try Code.line`
  yields `Nil` on both.

Two deliberate divergences remain, both cases where Rakudo reports a
compilation-unit artifact rather than the user's source:

- A `WhateverCode` (`* + 1`) reports line 1 and a *null* `Str` file in Rakudo
  (`.file.raku` actually dies with "chars requires a concrete string, but got
  null"). mutsu reports the real declaration line and file. mutsu's answer is
  strictly more useful, so it is kept and left out of the shared test.
- A `multi method` **dispatcher**'s `.line` in Rakudo points into the Metamodel
  (line 102 of the setting), not the user's class; mutsu reports the first
  candidate's line, consistent with how it treats a multi `sub` dispatcher.

`.file` for a routine declared in a `use`d module reports the module path in
both, but Rakudo appends the compunit name (` (ProbeMod)`). mutsu does not — and
mutsu's `$?FILE` inside a module does not either, so `.file` and `$?FILE` stay
consistent with each other. That suffix is a pre-existing, `$?FILE`-wide
difference, not something this change introduces.

## Coverage

`t/code-line-file-reflection.t` (27 assertions) pins named subs, multi-line
declarations, methods, submethods, inherited `.^lookup`, role methods, anonymous
subs, bare and pointy blocks, multi candidates, and the `.can`/`.^can` contract.
Every assertion is *relative* — line deltas, orderings, and
`.file.IO.basename` — so the file survives being moved or gaining a header, and
it passes verbatim under both `raku` and `mutsu`.

`Code.line`/`Code.file` also gained rows in the native method catalog
(`native_method_row_table.rs`), so `&f.^can('line')` and `Sub.^methods` report
them the way `name`/`signature`/`arity` already did.

No roast test exercises `Code.line`/`Code.file`, so the whitelist is unchanged.
