# `Code.line`/`Code.file` now answer for a `Regex` (grammar `token`/`rule`)

Follow-up to `news/2026-08/code-line-file-reflection.md`, which gave
`Code.line`/`Code.file` real answers for `Sub`, `Method`, `Submethod`, `Block`,
multi candidates, and multi dispatchers. `Regex` was the one `Code` subtype
left answering `Nil` for both:

```
$ mutsu -e 'grammar G { token foo { \d+ } }; say G.^lookup("foo").line.raku'
Nil
$ raku  -e 'grammar G { token foo { \d+ } }; say G.^lookup("foo").line.raku'
1
```

## The new field: `FunctionDef::source_line` now exists

`Sub`/`Method` both carry their declaration line on their own compiled body
(`CompiledCode::source_line`). A `token`/`rule` has no compiled body at all by
design (ADR-0009 keeps regex/grammar bodies interpreter-executed), so there was
nowhere on the routine to hang the line. The fix adds a new, symmetric home:
**`crate::ast::FunctionDef` now has a `source_line: Option<i64>` field**,
alongside its existing `source_file`. `Sub`/`Method` don't need to read it back
(they already have their own slot), but a `token`/`rule` — whose `FunctionDef`
lives in `Registry::token_defs` with `compiled: None` — has nowhere else to put
it.

**This is the same declaration-location slot the open `Backtrace::Frame`
tickets want** (they already read the sibling `source_file` field for frame
attribution). `FunctionDef::source_line` did not exist before this change; it
does now, and any future `Backtrace::Frame` work can read it directly instead
of growing a second, parallel channel.

## How the value gets into that field

Getting the value into `source_line` required a `SetLine`-tracking scan at
every place a token/rule declaration is classified, mirroring the scan
`Compiler::compile_method_body_keys` already does for a method's own
declaration line — a token/rule statement carries no line of its own; only the
`Stmt::SetLine` marker immediately preceding it in the enclosing statement list
does:

- `CompiledTokenDeclPlan` (`opcode.rs`) gained a `source_line` field.
  `CompiledCode::add_token_decl_plan` (top-level `my token`/`my regex`) stamps
  it from the compiler's `last_source_line` at the call site — the same value
  `compile_sub_body` reads for a named sub.
- `class_body_plan` (a class/grammar body's typed op mirror) was a plain
  `.map()` with no line-tracking state; it now tracks the running `SetLine`
  value across the flattened body the same way `compile_method_body_keys`
  does, so a `token`/`rule` declared directly in a `grammar`/`class` body gets
  its own line.
- `register_token_decl` threads the new `source_line` parameter into the
  registered `FunctionDef`; `make_native_method_object_ex_loc` (the `Regex`
  `Instance` builder `.^lookup` returns for a grammar token) reads it back off
  the `Registry::token_defs` entry instead of hard-coding `Nil`. A proto
  `token`/`rule` family with more than one `:sym<>` candidate reports the
  first-declared candidate's line, mirroring the existing multi-dispatcher
  convention.
- `&top-tok.line` (a bare reference to a top-level token, which resolves to a
  by-name `Routine` value rather than a `Regex` instance) needed the matching
  half: `routine_decl_location` only ever searched `registry().functions`
  (subs), so it now falls back to `Registry::token_defs` when no sub candidate
  carries a location.

## Beyond the ticket's own sketch: two findings worth calling out separately

The original ticket's fix sketch covered the class-body and top-level cases
above. Mapping the *full* surface (grammar token/rule, top-level declaration,
role composition, and grammar inheritance) turned up two more things, neither
of which the ticket asked for by name.

**1. A role-composed token would otherwise have silently kept `Nil`.**
A role's deferred body (`compile_role_deferred_body`) filtered `SetLine`
markers out entirely *before* this change — deliberately, to keep
`deferred_body_ops` empty when a role body has no real deferred statement at
all. Wiring only the class-body/top-level happy path would have left a
`token` declared inside a `role`, later composed into a `grammar`, still
answering `Nil`: the runtime side
(`run_composed_role_deferred_body`/`run_role_body_for_composition`) recompiles
the raw statement fresh at composition time, and a fresh `Compiler::new()` has
no line history to draw on. So `compile_role_deferred_body` now tracks the
running `SetLine` value first, then still drops the marker entries as before;
both composition consumers now call the token straight through a new
`register_token_decl_from_stmt` helper instead of recompiling, extracting the
declaration from the raw `Stmt` plus the precomputed line. Without this, the
role-composition row of the surface table below would still read `Nil`/file
only.

**2. `.^lookup` on an inherited grammar token was a pre-existing, independent
bug**, unrelated to line/file. It answered `(Mu)` — not `Nil`, not a missing
line, but nothing at all — even though the token dispatches correctly at parse
time (`Child.parse(..., :rule<inherited-tok>)` worked fine before this PR and
still does). The "check grammar token/rule/regex definitions" step of
`classhow_lookup_impl` looked the token up only under the receiver's own class
name, unlike the class-methods check just above it, which already walks the
full MRO. This bug existed regardless of the line/file work — it would have
blocked `Child.^lookup("inherited-tok")` from returning anything at all, line
or no line. It is now fixed alongside (walking the same MRO the class-methods
check uses), since it was the only way to actually verify the "inherited
token" row of the surface table, but it is a separate defect from — and would
be worth fixing even without — the `Code.line`/`Code.file` work in this PR.
`Child.^lookup("inherited-tok")` now finds the definition registered under the
declaring parent grammar, and reports that parent's `.line`/`.file`, matching
how an inherited *method* reports the declaring class's location.

## What was deliberately left unmatched

A bare `/.../` regex literal (`my $r = /\d+/`) reports line 1 and a *null*
`Str` file in Rakudo — a compunit artifact of how a top-level EVAL-like regex
literal is compiled, not a real declaration site worth reproducing. mutsu
reports `Nil` for both instead of chasing that null-string oddity; this
divergence was scoped out from the start (see the original ticket) and is not
covered by the new test.

## Surface verified against `raku` (in a real file, not `-e`)

| Site | raku | mutsu (before) | mutsu (after) |
|---|---|---|---|
| grammar `token`/`rule` (class body) | declarator line/file | `Nil`/`Nil` | matches |
| top-level `my token`/`my regex` | declarator line/file | `Nil`/`Nil` | matches |
| `token` in a role, composed into a grammar | role's declaration line/file | `Nil`/file only | matches (finding 1 above) |
| `token` inherited from a parent grammar | parent's declaration line/file | `.^lookup` itself returned `(Mu)` | matches (finding 2 above) |
| bare `/.../` literal | line 1, null `Str` file | `Nil`/`Nil` | `Nil`/`Nil` (unchanged, out of scope) |

## Coverage

`t/code-line-file-regex-token.t` (14 assertions) pins the grammar-body,
top-level, role-composition, and grammar-inheritance rows above, all as
relative facts (`$?LINE`-anchored deltas, `.file.IO.basename`), and passes
verbatim under both `raku` and `mutsu`.
