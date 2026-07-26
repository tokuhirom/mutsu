# A package named `Q` / `q` / `qq` / `m` is parsed as a quoting construct

Found 2026-07-26 while probing qualified calls (`Foo::bar()`); unrelated to that
work, and it predates it.

Raku lets a package be named after a quote language — `Q`, `q`, `qq`, `m` are
ordinary identifiers in name position. mutsu's parser takes them as the start of
a quoting construct instead, so the declaration and/or the qualified call blows
up:

```
$ mutsu -e 'module Q { our sub f() { 1 } }; say Q::f()'
Two terms in a row              # raku: 1

$ mutsu -e 'module q  { our sub f() { 1 } }; say q::f()'
Two terms in a row              # raku: 1

$ mutsu -e 'module qq { our sub f() { 1 } }; say qq::f()'
Two terms in a row              # raku: 1

$ mutsu -e 'module m  { our sub f() { 1 } }; say m::f()'
Runtime error: Unsupported use of /f. In Raku please use: a Raku adverb.
                                # raku: 1
```

`s`, `tr`, `rx`, `x`, `Z`, `X` are all fine, so this is specifically the
`Q`/`q`/`qq` bracketing-quote family plus `m` (which fails at the *call* site,
where `m::f()` is read as `m/…/`).

Two distinct fixes, both about where the quote slang may start:

1. **Name position after a declarator.** After `module` / `class` / `package` /
   `role` / `grammar`, the next identifier is a package name; the quote slang
   must not be entered there. That is what breaks `module Q { … }` — `Q {` is
   read as a `Q`-quote with `{}` delimiters, which also explains the error
   surfacing as "Two terms in a row" rather than a parse error at the keyword.
2. **Identifier followed by `::`.** `m::f()` / `Q::f()` is a package-qualified
   name; an identifier immediately followed by `::` is never a quote opener.

Fix (2) is the smaller and more general of the two, and would also cover
`my Q::Bar $x` and other name positions the declarator rule does not reach.

Worth doing because the failure mode is silent-ish and confusing (the reported
error names neither the package nor the real problem), and because `Q` is not an
exotic name — `Q::…` appears in the wild as a short namespace.

## Files

- `src/parser/` — quote-construct entry (the `Q`/`q`/`qq`/`m` openers) and the
  package-name parse after a declarator.
