# Slang host: `<arglist>`, `<statement>`, `<.kok>`, `<.set_braid_from>` and wrapped core methods

A slang grammar can now override the core methods only its own tokens call (`arglist`, `block`,
`circumfix:sym<( )>`, `<[ ]>`, `<{ }>`) and delegate to `$grammar.HOW.find_method($grammar, 'arglist')`;
the lookup answers a real `Method` (EEL tests `$original ~~ Method`) invoked with the cursor as its
invocant. The host grammar gained the `<arglist>` (a `RakuAST::ArgList`) and `<statement>` core rules,
rakudo's `kok`, and a no-op `set_braid_from`. Separately, `self` inside a token's method-call arguments
(`<.helper(self)>`) is now the grammar cursor instead of an error (#16888).
