# A quote-language name before `=>` is a pair key

`my %h = s=>1, m=>2, tr=>4, q=>5` and `C.new(s=>["a"])` with a later `=` on the
same line failed: the parser read `s=>…=` as a substitution (or `q=>…=` as a
quote) using `=` as the delimiter, closed by the later `=`. In Raku an
identifier followed by `=>` (horizontal whitespace allowed) is always an
autoquoted pair key, so the shared quote-language guard
(`quote_lang_shadowed`) now declines every quote construct — `s`, `m`, `tr`,
`y`, `q`, `qq`, `qw`, `qx`, `Q`, `rx`, `S`, `TR` — in that position, while
`s=a=c=` and `q=x=` keep using `=` as a delimiter. (#11371)
