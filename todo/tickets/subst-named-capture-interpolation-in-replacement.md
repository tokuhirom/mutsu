# `s///` replacement string doesn't interpolate named captures (`$<name>`)

Discovered via the doc-diff harness on `raku-doc/doc/Language/regexes.rakudoc` (around line
1816).

## Repro

```
$_ = '2016-01-23 18:09:00';
s/ $<y>=(\d+)\-$<m>=(\d+)\-$<d>=(\d+) /$<m>-$<d>-$<y>/;
.say;
```

- raku: `01-23-2016 18:09:00` (the named captures `$<y>`/`$<m>`/`$<d>` set in the pattern are
  interpolated into the replacement string)
- mutsu: `$<m>-$<d>-$<y> 18:09:00` — the replacement string is inserted **literally**, with none
  of the `$<name>` references interpolated

## Root cause guess

The `s///` replacement-string interpolation pass presumably handles plain scalar (`$var`) and
positional-capture (`$0`/`$1`) interpolation, but doesn't recognize/interpolate the
`$<name>`-postcircumfix-on-`$/`-implicit named-capture syntax when it appears directly in a
replacement string.

## Affected files (starting point)

- `src/vm/vm_string_regex_ops.rs` — substitution (`s///`) replacement-string interpolation
