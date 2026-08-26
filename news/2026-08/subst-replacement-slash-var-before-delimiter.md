# `$/` directly before a substitution's closing delimiter

Left over from making the `s///` replacement a real `qq` quote
(`news/2026-08/subst-replacement-is-a-qq-quote.md`): the *delimiter scanner*,
not the interpolation grammar, ended the replacement one character early when a
bare `$/` sat right before the close delimiter.

```raku
my $s = 'ab'; $s ~~ s/(a)/[$/]/;    # raku: [a]b -- mutsu: "Confused. expected statement"
my $s = 'ab'; $s ~~ s:g/<[ab]>/$//; # raku: ab   -- mutsu: "Regex not terminated."
```

`scan_to_delim_replacement` consumed the delimiter as part of `$/` only when a
`[`, `.` or `<` postfix followed it, so `$/.chars()`, `$/[0]` and `$/<k>` worked
while a bare `$/` did not.

## Establishing the rule

The ticket's worry was that treating `$` + delimiter as a variable
unconditionally would turn a working `s/foo$/bar/` into a parse error. It does
not, because the *pattern* half and the *replacement* half are different
languages and are scanned by different functions. In the pattern a trailing `$`
is the end-of-string anchor; in the replacement — a `qq` quote — there is no
competing reading at all. Rakudo v2026.06 confirms it by rejecting the
alternative outright:

```
$ raku -e 'my $c = "ab"; $c ~~ s/a/x$/; say $c'
===SORRY!=== Malformed replacement part; couldn't find final /
```

There is no "literal trailing `$`" replacement to protect. The one real
constraint is that the delimiter has to *spell* a variable. `s!a!$!!` uses `$!`
and works; `s,a,$,,` is diagnosed by Rakudo as "Non-variable $ must be
backslashed", because `$,` is not a Raku variable.

## The fix

The scanner now consumes `$` + close delimiter as one term whenever the
delimiter is one of the punctuation-named special variables (`/`, `!`, `$`) —
no postfix required. A delimiter that does not spell a variable keeps the old
behaviour of ending the replacement. `scan_to_delim` (the pattern half) is
untouched, so the end-of-string anchor is unaffected; the test pins
`s/foo$/X/` against both a matching and a non-matching subject.

Pinned by `t/regex-engine-gaps.t`.
