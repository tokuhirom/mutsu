# `$OUTERS::x = ...` stores, and `$?LANG` / `Perl6::Compiler.parsegrammar`

A write through `OUTERS::` (`$OUTERS::x = v`, `@OUTERS::x = ...`, `$OUTERS::x++`)
now reaches the innermost enclosing scope that declares the name, from a sub
and from a method body; it used to store nothing. `$?LANG` is the language
handle (`slang_grammar`/`slang_actions`), and `nqp::getcomp('Raku')` answers
`parsegrammar`/`parseactions` as getter/setters, which Jupyter::Kernel's
`Handler.update-compiler` calls after every evaluation. A method whose own
signature shadows the target name (`method TWEAK(:$x) { $OUTERS::x = $x }`)
still cannot reach it: method bodies are compiled without the enclosing
lexical chain.
