# `method dispatch:<.?>` (custom dynamic-dispatch method syntax) fails to parse

## Discovered while

Re-measuring `CSV::Table` after fixing the `@0`-in-array-literal parse bug
(`todo/tickets/numbered-capture-array-var-in-array-literal.md`, resolved —
see `news/2026-08/numbered-capture-array-var-in-array-literal.md`). With that
fix, `use CSV::Table` now gets past `Text::Utils` and into `Font::AFM`
further than before, but hits a new, unrelated parse failure.

## Repro

```
$ mutsu -I <Font-AFM lib> -e 'use Font::AFM'
Runtime error: Failed to parse module 'Font::AFM': Cannot add tokens of category 'dispatch'
```

Reduced from `Font::AFM.rakumod:594`
(`~/.zef/store/Font-AFM-1.24.10/*/lib/Font/AFM.rakumod`):

```raku
method dispatch:<.?>(\name, |c) is raw {
    ...
}
```

`dispatch:<.?>` is Raku's syntax for overriding a class's fallback dynamic
method dispatch (invoked when an ordinary `.?name` lookup would otherwise
fail) — a `method` declared with a `dispatch:<...>` "operator-style" name,
analogous to `method infix:<+>` for operator overloading but for the
dispatch mechanism itself (`FALLBACK`'s modern replacement/complement).
mutsu's parser does not recognize `dispatch:<...>` as a valid method-name
category.

## Root cause

Not yet investigated. Likely candidates:
- `git grep -n '"dispatch"' src/parser/` — check whether the method-name
  category parser (the one that already handles `infix:<...>`,
  `prefix:<...>`, `postfix:<...>` for operator overloading) has an
  allowlist of categories that is missing `dispatch`.
- Check `raku-doc/doc/Language/` for `dispatch:<...>` / custom dispatch
  documentation to confirm the full semantics before implementing (this is
  likely more than just a parser tweak — the VM/runtime method-dispatch path
  would need to actually invoke it as a fallback, similar to `FALLBACK`).

## Verification

- `mutsu -e 'class Foo { method dispatch:<.?>(\name, |c) is raw { name } }; say Foo.new.bar'` (or
  similar) should not error, and the fallback method should actually be
  invoked when an ordinary method lookup misses.
- Re-run `Font::AFM`'s own suite and `CSV::Table`'s suite under mutsu after
  the fix — keep going per `docs/batteries/csv.md`'s survey until either a
  further blocker is found or the suites pass.
