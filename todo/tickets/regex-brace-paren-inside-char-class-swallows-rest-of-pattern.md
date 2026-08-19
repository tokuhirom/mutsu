# A `{` or `(` inside a `<[...]>` char class swallows the rest of the regex

Reduced 2026-08-19 while working the `Template::Jinja2` row of
[todo/deep/template-engines-blocked-on-mutsu.md](../deep/template-engines-blocked-on-mutsu.md).
It is a **one-function** bug and it is the *entire* reason `Template::Jinja2`'s
`t/01-lexer.rakutest` fails 0/15 under mutsu.

## Repro

```raku
say ('Hello World' ~~ / <-[{]>+ /).Str;   # raku: Hello World   mutsu: H
say ('Hello World' ~~ / <-[(]>+ /).Str;   # raku: Hello World   mutsu: H
say ('Hello World' ~~ / <-[}{]>+ /).Str;  # raku: Hello World   mutsu: H
say ('Hello World' ~~ / <-[{]> .+ /).Str; # raku: Hello World   mutsu: H
say ('{{{x'        ~~ / <[{]>+  /).Str;   # raku: {{{          mutsu: (no match)
say ('Hello World' ~~ / <-[[]>+ /).Str;   # raku: Hello World   mutsu: H
```

Cases that already work and delimit the bug precisely:

| Pattern | mutsu | why |
| --- | --- | --- |
| `<-[}]>+` / `<-[)]>+` | OK | a lone *closing* brace/paren saturates the depth counter at 0 |
| `<-[{}]>+` | OK | the pair balances |
| `<-[<]>+` / `<-[>]>+` | OK | the `<` / `>` arms **already** carry a `bracket_depth == 0` guard |
| `<-[\{]>+` | OK | the backslash escape path bypasses the brace arm |

So the trigger is exactly: **an unbalanced opening `{` or `(` (or a literal `[`)
written inside an enumerated character class**.

## Root cause

`scan_angle_assertion_body()` in `src/runtime/regex_parse_core.rs` (the scan that
finds the closing `>` of a `< ... >` assertion) balances angles, parens, brackets
*and* braces so that an inner `>` cannot terminate the assertion early. The
`'<'` and `'>'` arms are correctly gated on `paren_depth == 0 && bracket_depth ==
0 && brace_depth == 0`, and the quote opener is gated on `bracket_depth == 0` —
but the `'('`, `')'`, `'{'` and `'}'` arms have **no `bracket_depth` guard at
all**:

```rust
'{' => { brace_depth += 1; name.push(ch); }
'}' => { brace_depth = brace_depth.saturating_sub(1); name.push(ch); }
```

Inside an enumerated char class `[...]` those four characters are ordinary
literals — Raku's char-class slang has no brace or paren nesting. So scanning
`<-[{]>+` goes: `[` → `bracket_depth = 1`; `{` → `brace_depth = 1`; `]` →
`bracket_depth = 0`; `>` → the guard sees `brace_depth == 1` and refuses to
close. The scan then runs to the end of the pattern and returns `closed: false`.
The caller retries once with `honor_quotes` off (same result) and ends up
treating the whole remainder as the assertion body, so `<-[{]>+ ` is parsed as a
negated class over the characters `{ ] > +` — **and the `+` quantifier, plus
every atom after the class, is silently swallowed**. That is why the match is one
character long and why `<-[{]> .+` loses its `.+`.

The `[` variant (`<-[[]>`) has the same shape via `bracket_depth` itself: a
literal `[` inside the class bumps the depth so the closing `]` never brings it
back to 0.

## Impact — measured

`Template::Jinja2` 0.2.0's lexer grammar is built on
`token chunk:sym<text> { <-[{]>+ || \{ <!before [\%|\{|"#"]> }`. Under mutsu the
`+` is lost, so every run of plain template text is chunked **one character per
token**: `tokenize('Hello World')` returns 11 `TOKEN_TEXT` tokens whose values
are `H`, `e`, `l`, … instead of a single `Hello World`. Escaping that one brace
by hand in a scratch copy of the dist (`<-[\{]>`) takes
`t/01-lexer.rakutest` from **0/15 to 15/15**, with no other change.

Nothing else in the dist is touched by the workaround, so the interpreter fix
alone unblocks that file. (The rest of the suite then hits a *different*
blocker — see
[qualified-private-method-call-uses-short-owner-name.md](qualified-private-method-call-uses-short-owner-name.md).)

`<-[{]>` / `<-[(]>` are completely idiomatic in template and config grammars, so
the blast radius is wider than one dist: any grammar that lexes "text up to the
next opening delimiter" hits it.

## Proposed fix

Add the missing `bracket_depth == 0` guards, mirroring what the `<` / `>` / quote
arms already do:

```rust
'(' if bracket_depth == 0 => { paren_depth += 1;  name.push(ch); }
')' if bracket_depth == 0 => { paren_depth = paren_depth.saturating_sub(1); name.push(ch); }
'{' if bracket_depth == 0 => { brace_depth += 1;  name.push(ch); }
'}' if bracket_depth == 0 => { brace_depth = brace_depth.saturating_sub(1); name.push(ch); }
```

(with the unguarded characters falling through to the `_ => name.push(ch)` arm).
That is the whole fix for the `{` / `(` cases and for the `Template::Jinja2`
blocker.

The literal-`[` case (`<-[[]>`) is a separate line in the same function and is
optional to fix in the same PR. It cannot be handled by "never nest `[`", because
`\c[LATIN SMALL LETTER A]` and `\x[263A]` legitimately put a bracketed argument
inside a class and rely on the current nesting to find their `]` (verified: mutsu
gets `<[\c[LATIN SMALL LETTER A]]>+` right today, and that must not regress). The
narrow rule is: while `bracket_depth >= 1`, only bump `bracket_depth` for a `[`
that immediately follows one of the bracket-taking escapes (`\c` `\C` `\x` `\X`),
and otherwise push it as a literal.

## Related, same theme, different scanner

`/ <[x] + [{]>+ /` dies at *parse* time with `Regex not terminated.` — a
source-level failure, before the runtime scanner ever sees the pattern.
`skip_char_class()` in `src/parser/primary/regex/scan.rs` only accepts a compound
class continuation when `+[` / `-[` follows the `]` **immediately**, with no
intervening whitespace; on a space it gives up, control returns to the main
delimiter scan, and the `{` in the second group is then mistaken for an embedded
`{ ... }` code block whose search for a closing `}` runs off the end of the file.
`<[x]+[{]>` (no spaces) parses, and `<[x] + [y]>` (spaces, no brace) parses, so it
takes both conditions. Worth folding into the same PR: let `skip_char_class()`
skip whitespace between a `]` and the following `+[` / `-[` / `>`.

## Pins to add

- `t/regex-char-class-literal-brace.t` — the six `~~` lines at the top of this
  file, plus the grammar form (`token TOP { <chunk>* }` with
  `token chunk:sym<text> { <-[{]>+ }` must produce **one** chunk for
  `'Hello World'`, not eleven).
- Keep an existing-behaviour assertion for `<[\c[LATIN SMALL LETTER A]]>+` so the
  optional `[` refinement cannot regress the escape form.

## Affected files

- `src/runtime/regex_parse_core.rs` — `scan_angle_assertion_body()` (the guards).
- `src/parser/primary/regex/scan.rs` — `skip_char_class()` (the related facet).
