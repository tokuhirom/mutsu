# RakuAST: `<a b  c>` keeps its raw text in `.AST`

`.AST` of a word list now equals rakudo's: `my @a = <a b  c>;` is
`QuotedString(processors => <words val>, segments => (StrLiteral("a b  c"),))`, with the
whitespace, the padding of `< a b >` and a newline inside the brackets kept. A single word is
a word quote as well, and a lone numeric literal (`<1/2>`, `<1+2i>`) stays a number term, as
in rakudo.

The parser used to turn the list into three one-word literals that cannot be told from
`'a', 'b', 'c'`. A parse that is asked to keep spellings (the `.AST` entry points and the
`MUTSU_RAKUAST` round trip) now wraps the term in the new `Expr::Spelled`
(`src/ast/spelled.rs`, [ADR-12199](../../docs/adr/12199-source-spelling-carrier-for-literal-terms.md));
every other parse builds the plain expression it always built, so the compiler, the
precompilation cache and the analyses never see the wrapper and no strip pass exists. One
parse-time shape check, the `handles <a b>` clause of an attribute, had to learn to look
through it (so `class C { has $.x handles <a b> }` now matches rakudo's `.AST` too).

On the `.AST` text corpus the `words-quote` class is at zero (94.7% of 11779 statements are
identical to rakudo's). The class used to count declared operator names (`sub infix:<foo>`) as
well; they are the existing `name-parts-colonpairs` class now. Heredocs and the bare-statement
prefixes (`gather STATEMENT`) are the next slices of the same ADR.

Tests: `t/rakuast/rakuast-word-list-spelling.t` and `t/rakuast/rakuast-source-forms.t`, both of
which also pass under raku.
