# RakuAST: regex character classes, quantifier ranges and separators

The source-level regex tree (ADR-0088) read every run of non-space
characters as one literal. So `/a.b/.AST` was `Literal("a.b")`, and the
RakuAST round trip of `/a\d/` matched a literal backslash followed by `d`.
Any regex the tree could not read was refused at `.AST`: 285 of the `t/`
files outside the round-trip ratchet stopped there.

A literal now holds only word characters, as in rakudo, where every other
glyph is a metacharacter. An escaped metacharacter joins the literal next to
it, so `a\#b` is the one literal `a#b`. A quantified literal splits off its
last character without a nested `Sequence`. The tree also models, measured
on rakudo 2026.09:

- the backslash classes `\d \w \s \n \h \v \t \e \f \r \0`, their negations,
  and `.` (`RakuAST::Regex::CharClass::*`);
- the `\x` / `\o` / `\c` escapes, which keep only the characters they denote
  (`CharClass::Specified`);
- enumerated classes such as `<[a..z]-[aeiou]>`, `<-alpha>` and
  `<+alpha -[x]>` (`Assertion::CharClass` with its `CharClassElement` and
  `CharClassEnumerationElement` nodes);
- the `**` ranges (`Quantifier::Range`), the `?` / `!` / `:` backtracking
  modifiers (`Backtrack::*` type objects) and the `%` / `%%` separators;
- the `:s` / `:r` internal modifiers and the `<<` / `>>` word boundaries.

Execution keeps the runtime parser's plan for every new node.

The wider tree exposed a split in how a stored regex boolifies. A regex
literal with a source tree captured its defining `$_`, and one without a
tree did not. Rakudo decides this by language version: under 6.c,
`Regex.Bool` matches the `$_` of the scope that boolifies it, and from 6.d
on, the `$_` of the scope that defines it. mutsu now follows the language
version for every regex literal. The 6.c rule is pinned by
`t/regex/rx-value-bool-map-topic.t`, and the 6.d rule by
`t/regex/rx-value-bool-defining-topic.t`.

`\E` and a wrong `\R` in the runtime parser are filed as #11444.
