# A delimited Pod block ends at the `=end` at its own indentation

The parser's Pod skipper ended a `=begin TYPE` block at the first `=end TYPE`,
whatever its indentation, so code that rakudo reads as the text of a block ran:

```raku
=begin code
  x
  =end code
say 42;     # mutsu ran this; rakudo keeps it in the code block
=end code
```

As in rakudo, the closing `=end` is now the one at the `=begin`'s own
indentation. Inside a verbatim block (`code`, `input`, `output`, `comment`) an
indented `=begin`/`=end` is text; inside any other block a same-type `=end` at
another indentation, and an `=end` no `=begin` opened, are rakudo's RakuDoc
syntax error (`X::Syntax::InfixInTermPosition`, "An error in RakuDoc syntax is
suspected..."). The delimited-block skipper moved to `src/parser/pod_delimited.rs`
(#18369).
