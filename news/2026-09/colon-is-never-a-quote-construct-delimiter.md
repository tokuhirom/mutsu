# A `Pkg::sub()` for a single-letter package no longer misparses as a quote-op

```raku
package S {
    our sub foo() { 42 }
}
my $a = S::foo();
my $b = S::foo();
say $a;
say $b;
```

`raku` prints `42` twice. mutsu printed nothing — the whole rest of the
file was silently swallowed into one bogus expression, with no error and
exit code 0.

A package spelled `S` (or `s`, `m`, `ss` — anything colliding with a
quote-like operator sigil) broke as soon as its qualified sub was called a
*second* time anywhere in the file. `--dump-ast` on the failing case showed
the second `S::foo()` collapsed into a single `NonDestructiveSubst` node
(mutsu's AST node for `S/pattern/replacement/`) whose `pattern` was empty
and whose `replacement` was literally `"foo();\nsay S"` — the parser had
read the callsite's `::` as the substitution operator's own delimiter pair.

`S:` (`S` immediately followed by `:`) tries to parse as an adverb first
(`parse_match_adverbs`); when what follows the `:` isn't a valid adverb
name — as with the *second* `:` of `S::foo()`'s own `::` — nothing is
consumed and the bare `:` is left as the candidate opening delimiter. The
`is_delim` check that decides whether a following character can open the
`S///`/`s///`/`ss///`/`m//` literal accepted a bare `:` for all four of
those constructs. Real raku's grammar flatly rejects this ("Colons may not
be used to delimit quoting constructs" — confirmed against `raku` directly:
`S:5:abc:xyz:` and `s:x:hello:world:` both fail to compile with that exact
message), and `rx//` already excluded `:` from its own `is_delim` check in
this same file with a matching comment — `s`/`S`/`ss`/`m` just hadn't been
given the same exclusion. `tr`/`TR` and `Q`/`q`/`qq` were independently
already safe (verified): `tr`/`TR`'s adverb loop bails out via a
zero-length name check on a bare `:`, and `Q`/`q`/`qq` already guard
`rest.starts_with("::")` explicitly.

`t/modules/quote-colon-delim-package-qualified-collision.t` pins
`Pkg::sub()` for packages named `S`, `M`, and `Q`, called more than once,
alongside the colon-adverb forms of `s`, `m`, and `ss` (`s:g/.../.../`,
`m:i/.../`, ...) to confirm those still work — `S///` itself is not
retested here since `package S {...}` earlier in the same file legitimately
shadows it for the rest of the compilation unit in `raku` too (its own
quote-language-shadow rule, unaffected by this fix); the existing `t/regex/`
and roast substitution/transliteration suites already cover `S///`'s own
behavior end to end.

[#8363](https://github.com/tokuhirom/mutsu/issues/8363)
