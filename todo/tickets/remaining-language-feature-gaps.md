# Remaining language-feature gaps that no roast file whitelists

Extracted from PLAN.md §4 (2026-08-02). These are real spec gaps, but none of them flips a roast
file to passing on its own — which is why they never got picked up. Grouped here so they stay
visible without occupying the plan outline.

## 1. Multi-line feeds

A feed spanning lines with a leading `==>` does not parse. The blocker is the
`!ws_before.contains('\n')` guard in `parse_list_infix_loop`. Single-line feeds and `ff` / `fff` are
done.

```raku
my @r = (1, 2, 3)
    ==> map({ $_ * 2 })
    ==> sort();
```

`==>>` / `<<==` and `~<` / `~>` are unimplemented/unspecified **in Rakudo itself**, so they cannot be
started (no oracle).

## 2. Typed-exception gaps needing compile-time scope analysis

- strict-mode undeclared-variable detection
- cross-`EVAL` detection of class redeclaration
- `X::Redeclaration::Outer`

All three need compile-time scope analysis that mutsu does not currently perform; each is
non-trivial on its own.

## 3. `exits-ok($code, $exit, $reason)`

A `Test` routine documented in `raku-doc/doc/Type/Test.rakudoc` (effective with Rakudo 2026.01) that
mutsu does not provide: passes if the code exits with the given exit code. Implement alongside the
sibling `dies-ok` / `lives-ok` — in the **Test-module handler**, not as a core builtin (it is not in
`perl-func.rakudoc`). No roast file uses it (not in upstream roast HEAD either), so this is
Test-completeness / batteries polish.

## 4. `:D` / `:U` DefiniteHow coercion

`6.c/APPENDICES/A04-experimental/01-misc.t` sits at 16/19 on this (`Target:D(Source:U)`). Tracked
with the file in [TODO_roast/BLOCKERS.md](../../TODO_roast/BLOCKERS.md); listed here only so the
feature name is searchable.
