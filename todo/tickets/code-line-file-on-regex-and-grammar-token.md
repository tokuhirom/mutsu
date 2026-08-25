# `Code.line` is `Nil` for a `Regex` literal and a grammar `token`/`rule`

Follow-up to `news/2026-08/code-line-file-reflection.md`, which gave `Code.line`
and `Code.file` real answers for `Sub`, `Method`, `Submethod`, `Block`, multi
candidates and multi dispatchers. The one `Code` subtype still answering `Nil` is
`Regex`.

## Current behaviour

```
$ target/debug/mutsu -e 'grammar G { token foo { \d+ } }; say G.^lookup("foo").line.raku; say G.^lookup("foo").file.raku'
Nil
Nil
$ raku -e 'grammar G { token foo { \d+ } }; say G.^lookup("foo").line.raku; say G.^lookup("foo").file.raku'
1
"-e"
```

(raku's `1`/`-e` above is for the one-liner; in a real file raku reports the
`token` declarator's line and the file. A bare `/.../` literal reports line 1 and
a *null* `Str` in raku — a compunit artifact, not a target worth matching.)

Both accessors answer `Nil` rather than raising, so nothing is broken; the values
are simply unknown.

## Why it was not done in the same change

`Sub` and `Method` both carry their location on their own `CompiledFunction`
(`CompiledCode::source_line` + `CompiledFunction::source_file`), which is what
made that fix a three-line compiler stamp plus one constructor read. A `token` /
`rule` has no compiled body at all by design (ADR-0009 keeps regex/grammar bodies
interpreter-executed), so there is nowhere on the routine to hang the line today:

- A `token`/`rule` is registered as a `FunctionDef` with `compiled: None`
  (`registration_sub::register_token_decl`) into `Registry::token_defs`, not
  `Registry::functions` — so the `routine_decl_location` registry walk added for
  `Routine` values does not see it.
- `FunctionDef` has a `source_file` field but **no `source_line`** field.
- `CompiledTokenDeclPlan` (`src/opcode.rs`) carries only name/params/`raw_body`;
  `classify_class_body_stmt` builds it from a pure `.map()` over the type body
  with no `Stmt::SetLine` state, and `build_token_decl_plan` is documented as "a
  pure function of the raw statement, needing no compiler state".
- The token body itself contains no `SetLine` marker (verified with
  `--dump-ast`); only the *enclosing type body* does.

## Sketch of the real fix

1. Add `source_line: Option<u32>` to `crate::ast::FunctionDef` (with
   `#[serde(default)]`), next to the existing `source_file`. ~13 struct-literal
   construction sites to touch. This is the symmetric home and would also make
   the sub path robust when `FunctionDef::compiled` is `None`.
2. Add `source_line` to `CompiledTokenDeclPlan`; make `class_body_plan` (and
   `CompiledCode::add_token_decl_plan` for a top-level `token`) a `SetLine`-
   tracking scan instead of a `.map()`, exactly the way
   `Compiler::compile_method_body_keys` now does for methods.
3. Thread it through `register_token_decl` into the `FunctionDef`.
4. Have `make_native_method_object_ex` (which builds the `Regex` `Instance`
   `.^lookup` returns for a grammar token) read `line`/`file` from the
   `Registry::token_defs` entry instead of hard-coding `Nil`.

## Priority

Low. No roast test exercises `Code.line`/`Code.file` at all (verified by grep
over the whole vendored suite), and grammar introspection by source location is a
niche use. Worth doing when `FunctionDef::source_line` is wanted for another
reason — e.g. the open `Backtrace::Frame` tickets, which read the same
declaration-location slot.
