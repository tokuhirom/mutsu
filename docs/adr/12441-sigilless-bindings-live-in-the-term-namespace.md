# ADR-12441: Sigilless bindings live in the term namespace

- **Status**: Accepted (2026-10-15). Implemented with the issue's PR.
- **Date**: 2026-10-15
- **Deciders**: tokuhirom, Claude
- **Issue**: [#12441](https://github.com/tokuhirom/mutsu/issues/12441)
- **Related**: [#9962](https://github.com/tokuhirom/mutsu/issues/9962) (the term namespace for
  sigil-less constants, `src/term_names.rs`), [#11994](https://github.com/tokuhirom/mutsu/issues/11994)
  / #12434 and #13540 (the shadow-copy workaround this ADR retires)

## 1. Context

In Raku `\t` (a sigilless parameter, `my \t`, a pointy `-> \t`) declares the TERM `t`, and `$t`
declares a scalar. They are different symbols: `sub f(\t, $t) { say t, $t }; f(1, 2)` prints `12`.

mutsu stores a scalar `$t` sigil-stripped, under the key `t`, in a compiled frame's `local_map`
and in the runtime `env`. A sigilless binding was stored under the same key, so the two symbols
were one: `sub f(\t, $t)` printed `22`, `for 1 -> \t { my $t = 5; say $t + t }` printed `10`.

#9962 already fixed the same collision for sigil-less `constant`s by giving them a storage key
of their own, the term key `\t` (`term_names::term_key`); term resolution (a bare `t`, `::('t')`,
`MY::<t>`, exports, `EVAL`) translates between the spelling and the key. Sigilless bindings were
left on the scalar key, with a `TODO` in `src/term_names.rs`.

In the meantime the collision was patched from the declaration side: when a `my $t` was
compiled while a sigilless `t` was visible, the compiler copied (#12434) or bound (#13540) the
sigilless value into a slot under the term key and redirected bare `t` reads there
(`shadow_sigilless_term`, `shadowed_sigilless_terms`, `CompiledCode::shadowed_sigilless_reads`).
That covered only a `my $t` declaration the compiler could tell apart from a synthesized one; a
`$t` *parameter* never went through it, and neither did a pointy loop parameter. The compiler
also carried `sigilless_declared` (which sigilless names the program wrote itself) and
`sigilless_param_shadows_outer_scalar` (forcing an enclosing `$t` capture past a `\t` parameter
that took its key) for the same reason.

## 2. Decision

**A sigilless binding is stored under its term key `\name`, exactly like a sigil-less constant.**
The spelling stays `name` everywhere a program can observe it (`Parameter.name`, signature
`.raku`/`.gist`, error messages, named-argument keys); only the storage key changes.

The key is decided once per declaration form:

| declaration | where the key is chosen |
| --- | --- |
| routine / block / method parameter `\x`, `+x`, `\|x` | `ParamDef::storage_name()`; the compiler declares the slot under it (`declare_param`, `record_param_local_slots`), the binder binds under it (`binding_name`), and `precompute_param_name_syms` / `SubData::param_name_syms` bake it for the light call paths |
| `my \x` (any expansion carrying `MarkSigilless` / `MarkSigillessReadonly`) | the compiler's `decl_is_term` flag, set by the synthetic-block compilers for the `VarDecl` the marker names, makes the `VarDecl` arm use the term key as its storage name (the same path a `constant` takes) |
| `with` / `if` pointy `-> \x` | `decl_is_term`, set by `compile_if_binding_decl` |
| `for ... -> \x` (single) | `compile_for_construct` keys the parameter by its `ParamDef` before anything uses it |
| `for ... -> \k, \v` (multi) | the parser already spells these `\k`, which *is* the term key; the compiler stops stripping the backslash |

Every reader follows from the key:

- **Compiler.** `sigilless_locals` / `enclosing_sigilless` stay sets of *spellings* (what the
  bareword `t` means here). A bare `t` naming one reads the slot or the by-name binding of
  `term_key(t)`; a sigilless assignment target (`t = 5`) and lvalue root (`t[0] = 1`) resolve
  to the same key (`names_sigilless_binding`, `sigilless_storage_key`). The runtime ops a
  declaration emits (`MarkSigillessBind`, the `__mutsu_sigilless_readonly::` marker) carry the
  key, so the alias / readonly metadata is keyed `\t` too and never applies to `$t`.
- **Runtime.** Everything below the compiler already keys by whatever name the compiled code
  hands it: env↔slot sync, closure capture (`compute_free_vars` sees `\t` in `GetGlobal`),
  thread clones, block-scope restore, the alias chain. Term resolution already looks up `\t`
  for constants, so `EVAL 't'`, `MY::<t>` and `::('t')` find a sigilless binding unchanged.

The shadow machinery is deleted: `shadow_sigilless_term`, `emit_shadowed_sigilless_read`,
`shadowed_sigilless_terms`, `CompiledCode::shadowed_sigilless_reads`, `sigilless_declared` and
`sigilless_param_shadows_outer_scalar`. Nothing shares a key any more, so there is nothing to
shadow.

Sigilless and constant terms share one namespace on purpose: in Raku both are terms, a nested
`my \c` shadows an outer `constant c`, and a same-scope redeclaration of either is an error.

## 3. Alternatives considered

- **Keep the declaration-side shadow and extend it to parameters.** Every new declaration form
  (a `$t` parameter, a pointy loop parameter, a destructured sub-signature) would need its own
  hook, and the bound slot still aliases a key a later `my $t` reuses (`alloc_local` returns the
  existing same-named slot: the dead end recorded on #12441). Rejected: it is the band-aid this
  ADR removes.
- **Rename in the parser.** Spelling the declaration and its uses `\t` in the AST would make
  every downstream key right for free, but the parser cannot scope it (`register_sigilless_terms`
  is not lexical; a bare `t` is always `Expr::BareWord("t")`), and every reader of `ParamDef.name`
  for display or named-argument matching would have to strip it. Rejected.
- **Move scalars instead (`$t` keyed `$t`).** Scalars are by far the larger population (every
  `local_map` lookup, every runtime `env` probe, precompiled code, ecosystem modules' `MY::`
  expectations); the term namespace already exists. Rejected.

## 4. Consequences

- `sub f(\t, $t)`, `-> $t` inside a `\t` scope, `for ... -> \t { my $t }` and a `\t` bound to a
  container that changes after a `my $t` all behave as in Rakudo.
- A sigilless parameter's slot is named `\t` in `CompiledCode::locals`; tooling that prints
  locals shows the key.
- A routine's type capture (`::T`) is not a sigilless binding and keeps its plain key; a `for`
  loop's (`-> ::T $x`) is bound like a `my \T` and so moves with it.
- A parametric role's sigilless type parameter (`role R[\u]`) is bound under its term key by the
  binder and handed to the role body under its spelling, as before; telling `\u` from a `$u`
  parameter of the same role is left for a follow-up.
- Everything that rewrote a `Stmt::Assign` into an `Expr::AssignExpr` (the tail-value paths, the
  map/grep recompile, statement-level feeds) carries the term key for a sigil-less target: an
  `AssignExpr` named by a plain spelling is a `$` scalar.
