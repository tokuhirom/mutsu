# A regex-embedded `:our $var = ...;` declarator doesn't write back to the package variable

Discovered via the doc-diff harness on `raku-doc/doc/Language/regexes.rakudoc` (around line
1612).

## Repro

```
grammar HasOur {
    token TOP {
        :our $our = 'thor';
        $our \s+ is \s+ mighty
    }
}
say HasOur.parse('thor is mighty');
say $HasOur::our;
```

- raku: `｢thor is mighty｣` then `thor`
- mutsu: `｢thor is mighty｣` then `Nil`

The match itself succeeds (so the `:our $our = 'thor';` declaration is at least usable *within*
the token — `$our` correctly matches the literal text), but the package variable
`$HasOur::our` isn't updated afterward.

## Root cause guess

`:our $var = ...;` inside a regex/token presumably creates a lexical binding for use during
matching (which works — that's why the match succeeds), but doesn't additionally write the value
through to the package's `our`-scoped storage the way a plain (non-regex) `our $var = ...;`
declaration would.

## Affected files (starting point)

- `src/runtime/regex.rs` / `src/parser/` — regex-embedded `:our` declarator handling
- Compare to how a plain top-level `our $var = ...;` writes to package storage

## Suggested next step

Check whether the regex-embedded declarator path even attempts a package-scope write, or skips
it entirely (treating `:our` the same as `:my` internally, which would explain the exact
symptom observed).
