# Grammar `.^method_table` lists tokens, and a Regex's gist is its declaration

`Grammar.^method_table` used to be empty for a grammar: its `token`/`rule`/`regex`
declarations live in the token registry, not the class method table. It now lists them
(including `proto token` dispatchers, also when they arrive through a role composed into
another role), and each entry gists as its verbatim declaration text (`token love { '<3' | love }`)
the way Rakudo does. The text is recorded by the parser on the regex value and kept across the
module AST cache through a new `SerValue::RegexDeclared` form.

Two smaller fixes fell out of running `Grammar::TokenProcessing` (random sentence generation
reads a grammar's rules this way): `@a.grep(...)>>.&sub` no longer answers an empty list (the
dynamic hyper path never reified the deferred Seq), and a `proto token` in a role body is now
registered under the composing grammar.

`Grammar::TokenProcessing` t/03 now passes. Residue: #10632 (a derived class's named-only
multi method candidate loses to a parent's positional `UInt` one, blocking t/05) and #10633
(dynamic `GrammarHOW.new_type` grammars, t/06).
