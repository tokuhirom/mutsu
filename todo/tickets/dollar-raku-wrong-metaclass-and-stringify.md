# `$*RAKU` reports the wrong metaclass name (`Perl` instead of `Raku`) and inconsistent stringification

Discovered via the doc-diff harness on `raku-doc/doc/Language/variables.rakudoc` (around line
1765).

## Repro

```
$*RAKU.put;
say $*RAKU.^name;
say $*RAKU;
```

- raku: `$*RAKU.put` prints `Raku`
- mutsu: `$*RAKU.put` prints `Perl()`; `$*RAKU.^name` is `Perl` (should be `Raku`); plain
  `say $*RAKU` gives `Raku (6.d)` — inconsistent with both of the above, which still reference
  the legacy `Perl` name

## Root cause guess

`$*RAKU`'s underlying type is presumably still named/tagged internally as `Perl` (a pre-rename
leftover — the compiler identity object was called `Perl` before the Perl-6-to-Raku rename), and
only some of its stringification paths (`.gist` used by plain `say`) were updated to say "Raku"
while `.^name` and `.put`/`.Str` still reference the old internal type name and produce an
inconsistent `Perl()` render.

## Affected files (starting point)

- `src/runtime/` — wherever `$*RAKU`'s backing type/class is registered (grep for `"Perl"` as a
  class/type name in the compiler-identity object's registration)

## Suggested next step

Grep the codebase for the literal type name `"Perl"` used for this compiler-identity object and
rename it to `"Raku"` consistently, then verify `.^name`/`.put`/`.Str`/`say` all agree.
