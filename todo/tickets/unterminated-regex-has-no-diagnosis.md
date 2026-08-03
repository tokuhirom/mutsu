# An unterminated regex literal has no diagnosis, so it lands on whatever parse happened to fail next

rakudo answers *every* unterminated regex the same way — an `X::Comp::Group`
whose sorrow is `Regex not terminated.` and whose panic names the delimiter it
could not find:

```
'x' ~~ m/foo             X::Comp::Group  Regex not terminated. / Couldn't find terminator /
'x' ~~ /foo              X::Comp::Group  Regex not terminated. / Unable to parse regex; couldn't find final '/'
my $r = rx/abc           X::Comp::Group  Regex not terminated. / Couldn't find terminator /
'x' ~~ m/foo (#) bar /   X::Comp::Group  Regex not terminated. (twice) / Unable to parse expression in metachar:sym<( )>
```

mutsu gives three *different* wrong answers for the first three, because
`scan_to_delim` (`src/parser/primary/regex/scan.rs`) returns `None` and the
regex-literal parse simply backtracks; whatever alternative parse fails last
supplies the exception:

```
'x' ~~ m/foo             X::Undeclared::Symbols
'x' ~~ /foo              X::Syntax::Confused
my $r = rx/abc           X::Str::Numeric
```

The fourth is the one roast asks about directly:
`roast/S05-metasyntax/regex.t` test 29 (`throws-like q['x' ~~ m/foo (#) bar /],
X::Comp::Group, 'commented capture end = parse error'`). mutsu's regex scanner
*does* honour `#`-to-end-of-line comments, so it correctly consumes `) bar /`
and finds no terminator — it just has nothing to say about it.

## Why it is not a one-liner

`scan_to_delim` returning `None` is load-bearing for backtracking: a bare `/` is
division far more often than it is a regex, so the scanner cannot be made to
throw. The commit point differs per opener:

* `m/…`, `rx/…`, `s/…/…/` — the keyword commits, so a failed scan there is
  unambiguously an unterminated regex and can throw.
* a bare `/…/` in term position — mutsu has already decided it is a regex by the
  time it scans, so it is probably safe too, but that decision is spread over
  the term-position dispatch and needs checking before committing.

So the work is: add the `Regex not terminated.` sorrow + a
`X::Comp::FailGoal` panic (via `PError::comp_group`, added in
`news/2026-08/comp-group-for-two-complaints.md`), and thread it out of the
committed openers only, one at a time, with the bare-`/` case measured last.

## The other file in the same cluster

`roast/S02-literals/quoting-unicode.t` also wants an `X::Comp::Group` (line 93,
`m\c[SNOWMAN].\c[COMET]`), but it is *not* blocked on this alone: it loses seven
assertions, six of them "Can't mix curly quote with ASCII quote" — arbitrary
paired-delimiter validation, a separate piece of work.
