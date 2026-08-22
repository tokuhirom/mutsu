# `anon class`/`anon sub` with a non-ASCII name fails to parse; `anon sub NAME` also gists without the `&` sigil

Discovered via the doc-diff harness on `raku-doc/doc/Language/variables.rakudoc` (around line
768).

## Repro 1 — non-ASCII name parse failure

```
say anon class þ {};
say anon sub þ  { 42 };
```

- raku: `(þ)` then `&þ`
- mutsu: `Runtime error: comma or statement end after argument`

With a plain ASCII name (`Foo` instead of `þ`), both declarations parse fine in mutsu — so the
parse failure is specific to a non-ASCII identifier following `anon class`/`anon sub`.

## Repro 2 — `anon sub NAME` gists without its `&` sigil (ASCII name, so parses; separate bug)

```
say anon sub Foo { 42 };
```

- raku: `&Foo`
- mutsu: `Foo`

A named anonymous sub's default stringification should include the leading `&` (matching how a
regular named sub gists), but mutsu drops it.

## Root cause guess

1. The identifier-parsing path used right after `anon class`/`anon sub` likely only accepts an
   ASCII identifier character class, unlike ordinary declaration-name parsing elsewhere (which
   presumably already supports non-ASCII identifiers, since raku itself allows them freely).
2. `anon sub NAME { }`'s default `.gist`/`.Str` is missing the `&`-sigil prefix that a normal
   named `Sub`'s stringification includes.

## Affected files (starting point)

- `src/parser/` — `anon class`/`anon sub` name-token parsing (character class accepted)
- `src/builtins/methods_0arg/` or wherever `Sub`/`Routine` default gist/stringify lives
