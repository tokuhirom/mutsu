# A quoted `<` / `>` inside a regex assertion no longer breaks the parse

`todo/tickets/grammar-named-capture-resolved-as-method.md` reported that
`Template::Mojo` 0.2.2 ran 5/5 of its test files under raku and 0/5 under mutsu,
every one dying with `No such method 'characters' for invocant of type 'Match'`,
and concluded that mutsu was turning a named-capture access into a method call.
The ticket also noted that a hand-written small grammar did not reproduce it.

It is not a named-capture bug at all. Reducing the real grammar gives a two-line
repro with no grammar, no actions and no capture:

```raku
say so ('ab' ~~ / <!before '%>' > . /);
# raku:  True
# mutsu: ===SORRY!=== Confused.
```

## Root cause

A `< ... >` assertion may contain a quoted literal, and that literal may contain
the angle brackets themselves — `<!before '%>' >` and `<!before '<%' >` are
ordinary Raku, and `Template::Mojo`'s grammar uses both. mutsu counted those
characters toward the `<`/`>` nesting depth in **two** places:

- `src/parser/primary/regex/scan.rs` — the scanner that finds where a regex
  literal ends. A quoted `>` closed the assertion early, so the rest of the
  regex leaked into the surrounding expression and the statement failed to parse
  (`===SORRY!===`).
- `src/runtime/regex_parse_core.rs` — the lookaround parser that slices out the
  assertion's inner pattern. With the scanner fixed, this one still cut the
  inner pattern at the quoted `>`, so `<?before '>' >` silently compiled to
  something that never matched (`False` where raku gives `True`), and
  `<!before '<%' || \n >` died at match time with "Unrecognized regex
  metacharacter >".

Both now skip a quoted span the way the surrounding code already does for
backslash escapes. The escaped forms (`<!before \> >`) are unaffected.

**The quote-skipping is deliberately limited to lookarounds** (`before`/`after`,
with or without a `?`/`!`/`.` prefix). Applying it to every `< ... >` construct
broke the vendored zef: in a word-list alternation (`< a ' b >`) or a character
class a quote character is just a literal, so skipping ahead to a "terminator"
that never comes swallows the rest of the regex. Only a lookaround's body is
itself a regex, where a quote really does open a string literal.

The reported symptom was a downstream effect: with the grammar's `perlexpr` and
`characters` tokens failing to parse, the `<characters>` subrule was never
registered, so `$<characters>` in the actions class fell back to a method call
on the `Match`.

Pin: `t/regex-assertion-quoted-angle.t` — 12 assertions verified against raku
first, covering a quoted `>` and `<` in single and double quotes, inside a
bracketed group, next to an alternation, the escaped forms that already worked,
and a grammar token using the same construct.

## Effect on Template::Mojo

From every test file dying immediately to most of the suite running:
`00-basic` 15/17, `01-template` 3/3, `02-complex` 1/1, `04-native-named` 1/1.

Two unrelated failures remain — `00-basic`'s two arity-message assertions and
`03-capture`'s `Use of Nil in string context` — recorded in
`todo/tickets/template-mojo-residual-failures.md`.
