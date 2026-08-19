# Fixed: literal `{`/`(`/`)`/`[` inside a `<[...]>` char class no longer swallows the rest of the regex

`scan_angle_assertion_body()` in `src/runtime/regex_parse_core.rs` (the scan
that finds the closing `>` of a `< ... >` regex assertion) tracked nesting
depth for angle brackets, parens, square brackets, *and* braces so that an
inner `>` (e.g. `<.foo(a => 1)>`) would not terminate the assertion early. The
`<`/`>` arms already guarded on `bracket_depth == 0` so they would not nest
when appearing as literals inside an enumerated character class
(`<[...]>`), but the `(`/`)`/`{`/`}` arms had no such guard: a literal `{` or
`(` written inside a char class (`<-[{]>`, `<-[(]>`) incorrectly bumped
`brace_depth`/`paren_depth`, the scanner ran past the assertion's real
closing `>`, and everything after it in the pattern was silently dropped. For
example `'Hello World' ~~ / <-[{]>+ /` matched only `H` instead of the whole
string.

Fixed by adding the same `bracket_depth == 0` guard to the `(`, `)`, `{`, `}`
arms, so they fall through to the plain literal-push arm while inside a
class. A related facet of the same bug — a literal `[` inside a class
(`<-[[]>`) — was fixed with a narrower rule: while already inside a class
(`bracket_depth >= 1`), a `[` only bumps the depth when it is the bracketed
argument of a `\c`/`\C`/`\x`/`\X` escape (`\c[LATIN SMALL LETTER A]`,
`\x[263A]`), and is otherwise treated as an ordinary literal class member.
This preserves the legitimate nesting those escapes need while no longer
misreading a literal `[` as opening a nested class.

A second, related bug in the delimiter-level scanner (`skip_char_class()` in
`src/parser/primary/regex/scan.rs`, used while locating the end of a
`/ ... /` regex literal) only recognized a compound character-class
continuation (`+[`, `-[`) when it followed the closing `]` of the previous
group with no intervening whitespace. `/ <[x] + [{]>+ /` (whitespace around
the `+`) died at parse time with `Regex not terminated.` because the scanner
gave up on the whitespace, fell back to the main delimiter scan, and then
misread the `{` in the second bracket group as an embedded code block whose
unterminated search for a closing `}` ran off the end of the file. Fixed by
letting `skip_char_class()` skip whitespace both between a `]` and a
following `+`/`-`, and between that `+`/`-` and its `[`.

This was the entire reason `Template::Jinja2` 0.2.0's lexer grammar
(`token chunk:sym<text> { <-[{]>+ || ... }`) tokenized every run of plain
template text one character at a time instead of as a whole chunk — the
blast radius is wider than one dist, though, since `<-[{]>` / `<-[(]>` are
idiomatic in any grammar that lexes "text up to the next opening delimiter."

Pinned by `t/regex-char-class-literal-brace.t` (15 assertions covering the
repro cases, the `\c[...]`/`\x[...]` nesting escapes that must not regress,
a grammar built on `<-[{]>+`, and both compound-class whitespace forms).
