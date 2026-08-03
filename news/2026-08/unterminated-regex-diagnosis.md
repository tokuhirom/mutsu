# An unterminated regex now says so, instead of landing on whatever parse failed next

rakudo answers *every* unterminated regex the same way — an `X::Comp::Group`
whose sorrow is `Regex not terminated.` and whose panic (`X::Comp::FailGoal`)
names the delimiter it could not find. mutsu gave three *different* wrong
answers for three unterminated regexes, because `scan_to_delim`
(`src/parser/primary/regex/scan.rs`) returned `None`, the regex-literal parse
simply backtracked, and whichever alternative parse failed last supplied the
exception:

| source | before | after |
| --- | --- | --- |
| `'x' ~~ m/foo` | `X::Undeclared::Symbols` | `X::Comp::Group` |
| `'x' ~~ /foo` | `X::Syntax::Confused` | `X::Comp::Group` |
| `my $r = rx/abc` | `X::Str::Numeric` | `X::Comp::Group` |
| `'x' ~~ m/foo (#) bar /` | `X::Syntax::Confused` (a 400-character "expected …" dump) | `X::Comp::Group` |

The last one is what `roast/S05-metasyntax/regex.t` test 29 asks for
(`throws-like q['x' ~~ m/foo (#) bar /], X::Comp::Group, 'commented capture end
= parse error'`). mutsu's regex scanner already honoured `#`-to-end-of-line
comments, so it correctly consumed `) bar /` and found no terminator — it just
had nothing to say about it.

## Why the fix is a commit point, not a scanner change

`scan_to_delim` returning `None` is load-bearing for backtracking: a bare `/` is
division far more often than it is a regex, so the scanner itself cannot throw.
The diagnosis is raised at the *call sites*, and only for delimiters that cannot
mean anything else once the keyword has been read — `/` and the four bracket
pairs:

* `rx<delim>…` and `m<delim>…` commit. The other delimiters `m//` accepts
  (`m-…-`, `m!…!`) stay recoverable, because `m-bar` is an ordinary identifier —
  the pre-existing statement-boundary check next to that path exists for exactly
  that ambiguity.
* the bare `/…/` form commits too: `regex_lit` is only reached in *term*
  position, where a leading `/` cannot be an infix division.

`PError::comp_group` (from `news/2026-08/comp-group-for-two-complaints.md`) grew
a `comp_group_with_panic` sibling so the panic can be a real `X::Comp::FailGoal`
carrying its `goal`, rather than the default `X::Comp::AdHoc`.

Pin: `t/unterminated-regex.t` — the four unterminated shapes, the group's
sorrow/panic contents, and three assertions that division is still division. It
passes under `raku` as well.

## The other file in the same cluster

`roast/S02-literals/quoting-unicode.t` also wants an `X::Comp::Group` (line 93,
`m\c[SNOWMAN].\c[COMET]`), and is **not** freed by this: it loses seven
assertions, six of them "Can't mix curly quote with ASCII quote" — arbitrary
paired-delimiter validation, a separate piece of work.
