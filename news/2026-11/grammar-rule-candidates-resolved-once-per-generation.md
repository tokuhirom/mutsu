# A grammar rule's candidates are resolved once per declaration state

`Interpreter::resolve_token_defs` finds the candidates of a token, rule or
regex by walking the current package's MRO and, for every ancestor, building
the qualified key, interning it and scanning the name's proto variants. A
`Grammar.parse` asked for its start rule more than once per call, so a
three-rule parse of `xy` repeated that walk on every parse: about 20k of its
139k instructions.

The answer depends only on the rule name, the package the lookup starts from,
the executing routine's package (the fallback scope), and the declarations. A
declaration moves the registry write generation, and every generation value
names one registry content, so the walk's result is now remembered per thread
for one generation (`runtime/token_defs_memo.rs`).

`G.parse('xy')` went from 138.9k to 116.1k instructions per parse (callgrind
slope, profiling build), and `Cro::Uri.add('/page/N')` — which parses twice,
once for the reference and once for the authority — from 2.853M to 2.801M.
