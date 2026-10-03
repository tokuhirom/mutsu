# A `$/` parameter survives a regex smartmatch

In rakudo a routine whose `$/` is a parameter, typically a grammar action method
`method term($/) { ... }`, keeps that `$/` when its body smartmatches a regex.
The match still answers `True`/`False`, but it does not replace `$/`, so `$0`
and `$<name>` keep reading the argument. mutsu overwrote it.

EBNF::Grammar's actions were affected:
```raku
method terminal($/) {
    make do given ~$/ { when $_ ~~ /^ <-['"]>+ $/ { "'$_'" }; default { $_ } }
}
```
The failed `when` match left `$/` as `Nil`, and `make` then died with
"expects $/ to contain a Match". The smartmatch opcode now restores a readonly
`$/` after the match. A routine without a `$/` parameter, or one that declares
`my $/`, behaves as before. All five of EBNF::Grammar's test files now produce
the same output as rakudo.
