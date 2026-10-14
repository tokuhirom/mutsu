# RakuAST renders interpolating `qqw` quotes and `:key«…»` colonpairs

Two word-quote spellings stopped at `.AST`. Both now render the way rakudo
2026.09 does and lower back to what the parser builds:

```raku
my $x = 'X';
qqw/a $x b/;                 # QuotedString(processors => ("words",), segments => ("a ", $x, " b"))
f(:args«green yellow»);      # ColonPair::Value(value => QuotedString(<quotewords val>))
```

The first is a `words` quote whose words are only known once its string has
been interpolated. The parser spells it as a call to a runtime helper, and
the converter reads that call as the node. The second is a colonpair whose
value is a `«…»` or `<<…>>` list. The colonpair parser now parses that value
with the term parser, so it keeps the source text a bare `«…»` keeps.

Five more `t/` files pass under `MUTSU_RAKUAST=1`. A `<<…>>` that nests its
own opener (`<< a <<b>> c >>`) still renders as one literal run.
