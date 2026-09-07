# The rustdoc surface is drained and gated

```
cargo doc --no-deps --document-private-items
# before: warning: `mutsu` (lib doc) generated 250 warnings
# after:  (none)
```

Nothing in `.github/workflows/` ran rustdoc, so this surface had never been
gated and had drifted for as long as the crate has had doc comments. The
2026-09-06 compile-warning sweep
(`news/2026-09/compile-warnings-in-unlinted-configurations.md`) closed the
rustc/clippy blind spots and deliberately left this one, because it is a
different and much larger docs-only diff. It had grown from the 210 warnings the
ticket recorded to 250 by the time it was worked.

## What the 250 were, and what each turned out to be

**39 `unclosed HTML tag`** — Raku syntax written in prose without backticks:
`%h<k>`, `Buf[uint8]`, `trait_mod:<is>`, `Vec<Expr>`, `>>op<<`,
`"  in sub <name> at <file> line <N>"`. Backticked in place, except three
pseudo-code blocks in `src/compiler/stmt.rs` (the `for <ELEM>` desugars) that
were indented three spaces — one short of rustdoc's code-block indent — and are
now fenced as ```` ```text ```` instead.

**15 `public documentation ... links to private item`** — `[`ValueRepr`]`,
`[`NanBox`]`, `[`ValueView`]`, `[`crate::repl_core`]` and friends. These name
things that really are private, so the fix is to stop linking and keep the code
span.

**4 `this URL is not a hyperlink` / `redundant explicit link target`** — two
bare `https://docs.raku.org/...` lines became `<...>` autolinks; two
`[`X`](super::X)` duplications lost the redundant target.

**192 `unresolved link`**, which split into three real causes:

1. **Raku and stack-diagram brackets read as links** (~50): `[NaN]`, `[&func]`,
   `[Z*]`, `[min]`, `[2;1]`, `@a[i][j][k]`, `Stack: [target, index, value]`,
   `locals[slot]`, `local[i]`. Never meant to be links; backticked.
2. **A real intra-doc link written without a path** (~120): `[`alloc_local`]`,
   `[`declare_local`]`, `[`set_env_with_main_alias`]` and about a hundred more.
   rustdoc does not resolve a bare *associated item* name, so each needed
   `Self::`. Applied mechanically, but only where the target is defined in the
   same file **and indented** (i.e. really is an associated item) — five bare
   names that turned out to be free functions or macros were left for the
   per-site pass.
3. **A path that was wrong, stale, or unnameable** (~20): `Compiler::…` and
   `Interpreter::…` links from module docs that needed the full
   `crate::compiler::Compiler::…` / `crate::runtime::Interpreter::…` path;
   `super::` paths that pointed at the wrong module (`wrap_divergent_literal`
   lives in `parser::primary::number`, `native_row_servable` in
   `builtins::native_method_row`); `Deref` that needed `std::ops::Deref`; two
   links wrapped across a line (`[`x`](Self::` + newline + `x)`), which rustdoc
   cannot parse at all; and several targets that no longer exist
   (`sync_user_method_entries`, `unmask_thread_redeclared_params`) or are
   `#[cfg(test)]` / inside a private module (`collect_cycles`, `NativeMethodRow`,
   `meta_bracket::cannot_meta_ternary_error`, `wasm_sched::pump`), where the
   right answer is to stop linking.

One cause was structural rather than per-site: `src/value/value_buf.rs` had both
a `///` doc on its `mod value_buf;` declaration and a `//!` doc in the file.
rustdoc merges the two and resolves the merged block in the **outer** scope, so
ten links that were perfectly valid inside the module reported "no item named
`buf_elems` in module `mutsu`". Dropping the duplicate `///` on the declaration
fixed all ten at once.

## The gate

With the backlog at zero, `RUSTDOCFLAGS="-D warnings" cargo doc --no-deps
--document-private-items` is now a step in the `lint-configs` CI job and a
fourth line in `make lint`. It is a configuration nothing else compiles:
rustdoc is the only pass that resolves `[`item`]` links and parses the prose as
Markdown, which is why both classes of defect could accumulate for years under
green clippy. `--document-private-items` because almost every item in this crate
is `pub(crate)`.

Adding the gate in the same change as the last of the fixes is deliberate — a
gate added over a non-empty backlog just turns every PR red.
