# ANTLR4::Grammar's corpus parses: Match list methods, inline `:!s` in rules, `'#'` in rules

`ANTLR4::Grammar`'s `t/02-corpus.t` now passes all 54 of its corpus grammars, in about 5 seconds
(rakudo takes about 14). Four mutsu bugs were behind this, and three of them are fixed here.

- **A Match's list methods iterate its positional captures.** A `Match` is a `Capture`, and `Any`'s
  list coercions and iteration methods all go through `self.list`, which for a `Capture` is its
  positional part. mutsu answered `.List`, `.Slip`, `.flat`, `.cache` and `.reverse` with the
  match's *string*. It answered `.Seq`, `.grep`, `.sort`, `.tail`, `.eager` and `.iterator` with the
  whole Match as one item. `.Array` returned a `List`. A `for` over a non-itemized Match
  (`for $/<LEXER_CHAR_SET> { … }`) goes through `.Slip`, so it looped once over the Match's text,
  and ANTLR4's `[by]` character set came out as `<[ [by] ]>` instead of `<[ b y ]>`. Positional
  `.pairs` keys are now `Int`s, as in rakudo.
- **An inline `:!s` / `:!sigspace` inside a `rule` now turns sigspace off** for the rest of its
  group. `rule grammarType { ( :!sigspace 'lexer' | 'parser' )? 'grammar' }` used to capture
  `"lexer "` with the trailing space. `rule` whitespace is injected as text before the regex engine
  runs, and that pass ignored the adverb. It now tracks the sigspace state per bracket group and
  restores it when the group closes.
- **A quoted `'#'` in a `rule` is a literal, not a comment.** The same text pass started a line
  comment at any `#` outside a code block, even inside quotes. So
  `<parserElement> ['#' <label=ID>]?` lost everything after `'#'`, and every ANTLR alternative
  label (`a | b # Label`) failed to parse.

`t/11`–`t/14` now pass along with `t/02`. Three findings are left for their own issues: a heredoc
with trailing code on its marker line makes nested-block parsing exponential (#9674, which is what
`t/10-basic-grammar.t` hits); a list-valued capture in an alternation branch that did not run is
`Nil` instead of `[]` (#9675, `t/03-corpus-compile.t` on `Lua.g4`); and `Class.new(:attr(Nil))`
keeps `Nil` instead of resetting to `Any` (#9676).
