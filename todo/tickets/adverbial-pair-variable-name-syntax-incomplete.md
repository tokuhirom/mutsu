# Adverbial-pair variable names (`$var:adverb<value>`) parsing is incomplete

Found by the doc-diff harness batch-3 re-run (`docs/doc-diff-backlog.md`,
`Language/syntax.rakudoc:354` and `:384`).

## Root cause hypothesis

Raku allows a variable's name to carry an "adverbial pair" component, written
`$identifier:adverb<value>` (or with `«...»`/`[...]`/`(...)` instead of `<...>` for the
value, or with the identifier itself replaced by `<...>` too, e.g.
`$take-me:<home>`). The declaration and every subsequent read/write must use a matching
name; reading it uses `$var:adverb<value>` again (with `<>`/`«»`/`[...]`/`(...)` all being
valid ways to spell the same lookup, and `(...)` allowing an arbitrary expression).

mutsu's parser only handles some spellings:
- `my $foo:bar<baz> = 'quux'; say $foo:bar«baz»;` works (angle-bracket declaration + double
  angle-bracket read both parse).
- `my $take-me:<home> = ...;` — a declaration where the *identifier itself* is `<...>`
  quoted — is a hard parse error (`Confused. expected statement...`).
- Interpolating a value into the adverb's value part
  (`say $a:foo«$c»;` where `$c` holds `42`) parses but evaluates to `Nil` instead of
  reading back the value stored under `$a:foo<42>`.

## Minimal repro

```raku
my $foo:bar<baz> = 'quux';
say $foo:bar«baz»;                               # OUTPUT: «quux␤» -- OK on mutsu

my $take-me:<home> = 'Where the glory has no end';
say $take-me:['home'];                           # OUTPUT: «Where [...]␤»
```

- `raku`: `quux`, `Where the glory has no end`, then `5` for a third `$foo:bar(1+1)` case.
- `mutsu`: hard parse error at `my $take-me:<home> = ...`:
  ```
  ===SORRY!=== Error while compiling ...
  Confused. expected statement: expected use statement or import statement or no statement or need statement or unit statement or ...
  ------>my $take-me:<home> = 'Where the glory has no end';
                    ^
  ```

Second, narrower repro (interpolated adverb value):

```raku
constant $c = 42;
my $a:foo<42> = "answer";
say $a:foo«$c»;    # OUTPUT: «answer␤»
```

- `raku`: `answer`
- `mutsu`: `Nil` (parses without error, but the lookup doesn't find the stored value —
  likely because `«$c»` interpolation of the adverb value isn't resolved to `42` before the
  variable-name match, or the two spellings resolve to differently-keyed storage).

## Affected files (starting point)

- Parser: wherever `$identifier:adverb<value>` variable-name syntax is recognized (grep for
  "adverbial" / colon-pair variable name parsing in `src/parser/`). Needs to accept
  `$<...>:adverb<...>` (identifier itself quoted) and to correctly interpolate/resolve the
  adverb value consistently between declaration and read sites.
