# `<same>` builtin regex subrule (repeat-previous-capture) is unimplemented

Discovered via the doc-diff harness on `raku-doc/doc/Language/regexes.rakudoc` (around line
1349). Already noted as an aside in `docs/doc-diff-backlog.md`'s Deferred section ("NB:
`regexes.rakudoc` [3] `<same>` is a *separate* missing builtin subrule, not this root") but was
never filed as its own ticket — filing it now.

## Repro

```
say '123345' ~~ m/ <same>\d+ /;
say 'aa11' ~~ m/ <alpha><same><digit> /;
```

- raku: `｢345｣` (with `same => ｢｣`) then `False`
- mutsu: `No such method 'same' for invocant of type 'Match'`

`<same>` is a builtin regex subrule that matches only if it repeats the immediately preceding
capture's matched text (a backreference-like assertion).

## Root cause

Simply unimplemented as a builtin regex subrule — `src/runtime/regex.rs` /
`src/runtime/regex_parse.rs`'s builtin-subrule table (alongside `<alpha>`, `<digit>`, etc.) has
no `<same>` entry.

## Suggested next step

Add `<same>` to the builtin regex subrule dispatch, implementing "match text identical to the
immediately preceding capture" (this needs access to the most recent capture's matched
substring at the point `<same>` appears, similar to how a numbered backreference like `$0` would
be resolved, but referring to whatever capture came immediately before rather than a specific
index).
