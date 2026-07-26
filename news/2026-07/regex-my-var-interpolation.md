# Match-time interpolation of an in-regex `:my` variable

A bare `$name` in a regex, where `$name` was declared by an in-regex
`:my $name …` declaration, is a Raku interpolation of that lexical's string value
as a literal — evaluated **at match time**. mutsu handled `$name` only via
`interpolate_bound_regex_scalars` / `interpolate_regex_scalars`, which substitute
the value from the outer `env` *before* matching. A regex-local `:my` var isn't
in `env` (its value is produced while matching, e.g. by a `VarDecl` atom or a
code block), so the substitution found nothing and lowered `$name` to an
always-fail (`<!>`) — the interpolation never matched.

Now a bare `$name` naming an in-regex `:my`/`:let` var lowers to a new
`RegexAtom::VarInterp(name)` atom that, when matched, reads the value from the
running capture store's `regex_vars` (falling back to `env`) and matches its
string form literally — like a `NamedBackref` reads a capture. An undefined value
interpolates as the empty string (a zero-width match). The two interpolation
pre-passes now leave a `:my`-declared name verbatim for this lowering instead of
substituting it.

```raku
grammar G { token TOP { :my $v = 'ab'; $v 'c' } }
G.parse("abc")   # now matches (was: never)
```

This is a prerequisite for the YAMLish battery's block collections, whose
indentation-tracking `root-block` captures an indent string into a `:my` var and
re-matches it on continuation lines. (The remaining half — running a `{ … }`
side-effect block *inline* during matching so it can compute that var — is
tracked separately in `todo/deep/yamlish-block-collections-regex-vars.md`.)

Known limitation: when an outer lexical shares the exact name of the regex-local
`:my` var, mutsu can still pre-substitute the outer value on a secondary parse
path; the common (non-shadowing) case — which is what real grammars use — is
correct. Pin: `t/regex-my-var-interpolation.t`.
