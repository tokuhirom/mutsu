# `Regex.Str` warns and returns the empty string

```raku
say (/a/).Str.raku;
# raku:  Regex object coerced to string (please use .gist or .raku to do that)
#        ""
# mutsu: "/a/"
```

Two things differed: raku emits a warning, and it yields the **empty string**
rather than the regex's source text. `.gist` and `.raku` legitimately show the
source — the two consumers simply shared one answer (`to_string_value`).

## Why it mattered beyond the coercion

A smartmatch of a `Regex` against a `Regex` stringifies the LHS to give the RHS
a subject, which is what a self-match of a regex-valued topic does:

```raku
for (/a/,) { say ($_ ~~ $_).raku }
# raku:  Nil     mutsu: Match.new(:orig("/a/"), :from(1), :pos(2))
```

raku's `""` cannot contain an `a`, so the match fails and the smartmatch is
`Nil`. mutsu's `"/a/"` *does* contain an `a` at offset 1, so it reported a
spurious `Match`.

## The fix

`Interpreter::regex_str_coercion` is the one answer, built on the existing
`raise_resumable_warning` (so the warning goes to stderr with rakudo's wording,
is suppressible by `quietly`, and is catchable by a `CONTROL` handler like every
other resumable warning). It is consulted at each string-coercion consumer,
which the codebase already had separated from the `.gist` ones:

- `.Str` / `.Stringy` (the pure fast path now declines a `Regex`, since the
  warning needs the interpreter);
- prefix `~`;
- infix `~` and the string comparisons, through `coerce_stringy_operand`;
- interpolation (`StringConcat`);
- `print` and `put`;
- and the smartmatch subject, `regex_match_text`.

`say` is deliberately not among them: it renders `.gist`, so `say /a/` still
prints `/a/` with no warning — measured, along with every other row.

Pinned by `t/regex-str-coercion-warns.t`, whose 14 assertions pass unchanged
under rakudo; three of them use `is_run` to check that the warning really
reaches stderr, that `quietly` silences it, and that `say` does not emit it.
