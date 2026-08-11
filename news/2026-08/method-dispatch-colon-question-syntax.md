# Fix `method dispatch:<...>` (custom dynamic-dispatch method syntax) failing to parse

`method dispatch:<.?>(...)` — Raku's syntax for a `method`/`submethod`
declared with the operator-style name `dispatch:<...>`, which overrides a
class's `.?method` fallback dispatch — failed to parse with `Cannot add
tokens of category 'dispatch'`. mutsu's operator-category allowlist (shared
by `infix:<+>`, `prefix:<->`, `circumfix:<[ ]>`, etc.) was missing
`dispatch` entirely.

Verified against a live `raku` (2026.06) that `dispatch` is a category valid
**only** on a `method`/`submethod` declaration: `sub dispatch:<.?> {}` (even
`multi sub`) raises the exact same "Cannot add tokens of category
'dispatch'" error mutsu already raised for every unknown category, while
`method`/`submethod dispatch:<.?>` compiles. So the fix is scoped to the
method-declaration parse path only (`method_decl.rs`), via a new
`parse_method_sub_name()` that extends `parse_sub_name()` with an
`allow_dispatch` flag threaded through `parse_sub_name_inner()` and
`operator_name_extension_error()` — general `sub`/`token`/`regex`/etc.
declarations keep rejecting `dispatch` exactly as before. Also added
`dispatch` to the null-operator-symbol detection list (`method dispatch:<>`
now raises the same "Null operator is not allowed" / empty-list worry a
`method infix:<>` does), matching rakudo.

Pinned in `t/method-dispatch-colon-question-syntax.t`. The test only pins
the *parse* fix and that an ordinary declared method still dispatches
normally — it does not assert full custom-dispatch invocation semantics for
`.?`, since rakudo itself does not document them and the concrete blocker
(`Font::AFM.rakumod` parsing) does not exercise that code path at runtime.

## Discovered while

Re-measuring `CSV::Table` (a batteries CSV-slot candidate, see
`docs/batteries/csv.md`) after fixing the `@0`-in-array-literal parse bug
(`news/2026-08/numbered-capture-array-var-in-array-literal.md`):
`Font::AFM.rakumod:594` has `method dispatch:<.?>(\name, |c) is raw { ... }`.

## Residue

With this fix, `use Font::AFM` and `use CSV::Table` both load cleanly, and
8/10 of `CSV::Table`'s own test files run. Every file that actually
constructs a `CSV::Table` object now hits a new, unrelated blocker: a
transitive dependency (`Text::Utils` → `AlgorithmsIT`) assigns through a
user class's `return-rw AT-POS` via `$obj[$i] = v` (no `ASSIGN-POS`
declared), and mutsu silently drops the write instead of writing through the
returned container. Filed as
`todo/tickets/custom-at-pos-return-rw-index-assign.md`, with the root cause
already narrowed to `src/vm/vm_var_assign_index_named.rs`'s Instance-target
assign path. `CSV::Table` is still blocked; the next person picking this up
should start there.
