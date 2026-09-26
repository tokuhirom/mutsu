# Guide to the vendored Raku documentation (`raku-doc/`)

Which files in the vendored `raku-doc/` answer which question. `AGENTS.md` sends you here when the
language spec or a type's behavior is unclear; `raku -e` against the installed Rakudo is the other
oracle.

All documentation is under `raku-doc/doc/` in `.rakudoc` format (Pod6 markup, readable as plain text).

## Language reference (`raku-doc/doc/Language/`)

Core syntax and semantics — consult these when implementing or debugging language features:

- **`syntax.rakudoc`** — General syntax: literals, identifiers, statements, comments, special variables
- **`operators.rakudoc`** (135 KB) — **All operators with precedence levels**, associativity, and examples. Essential reference for parser precedence implementation.
- **`control.rakudoc`** — Control flow: if/unless/with/without, for, while, loop, given/when, repeat, gather/take, supply/react/whenever
- **`functions.rakudoc`** — Function definition, calling conventions, return handling
- **`signatures.rakudoc`** — Parameter syntax, types, constraints, slurpy params, destructuring
- **`variables.rakudoc`** — Sigils (`$`, `@`, `%`, `&`), twigils, special variables, dynamic scope
- **`regexes.rakudoc`** (120 KB) — **Complete Raku regex syntax**. Anchors, quantifiers, captures, lookahead/lookbehind, character classes, alternation, conjunction, etc.
- **`grammars.rakudoc`** — Grammar, token, rule, regex declarators; actions; `TOP` method; inheritance
- **`grammar_tutorial.rakudoc`** — Step-by-step grammar tutorial with practical examples
- **`quoting.rakudoc`** — Quoting constructs: `q//`, `qq//`, heredocs, word quoting (`<...>`, `qw`), interpolation rules
- **`objects.rakudoc`** — OOP: classes, roles, attributes, methods, inheritance, composition, MRO
- **`classtut.rakudoc`** — Class tutorial with practical examples
- **`typesystem.rakudoc`** — Type system: type objects, coercions, subsets, where clauses
- **`subscripts.rakudoc`** — Positional and associative subscript syntax (postcircumfix `[]`, `{}`, `<>`)
- **`list.rakudoc`** — Lists, arrays, sequences, flattening, itemization
- **`containers.rakudoc`** — Scalars, arrays, hashes as containers; binding vs assignment
- **`contexts.rakudoc`** — Sink, boolean, string, numeric, list context coercion
- **`terms.rakudoc`** — Term syntax: self, now, time, rand, empty, etc.
- **`brackets.rakudoc`** — Bracket pairs and nesting rules
- **`phasers.rakudoc`** — BEGIN, CHECK, INIT, END, ENTER, LEAVE, KEEP, UNDO, FIRST, NEXT, LAST, PRE, POST, QUIT, CLOSE, COMPOSE
- **`statement-prefixes.rakudoc`** — `do`, `try`, `quietly`, `gather`, `lazy`, `eager`, `hyper`, `race`, `sink`, `react`, `supply`
- **`traits.rakudoc`** — `is`, `does`, `handles`, `of`, `returns`, `will` trait modifiers
- **`slangs.rakudoc`** — Slang mechanism details (sub-language switching)
- **`setbagmix.rakudoc`** — Set, Bag, Mix types and operations
- **`numerics.rakudoc`** — Numeric types: Int, Num, Rat, FatRat, Complex; coercion rules
- **`exceptions.rakudoc`** — Exception handling: try, CATCH, die, fail, X:: types
- **`packages.rakudoc`** — Packages, modules, classes, roles as package types
- **`traps.rakudoc`** — Common pitfalls and gotchas (useful for understanding edge cases)

## Type reference (`raku-doc/doc/Type/`)

Per-type method documentation — consult when implementing methods on specific types:

- **`Test.rakudoc`** — **Test module specification**: `plan`, `ok`, `nok`, `is`, `isnt`, `is-deeply`, `is-approx`, `like`, `unlike`, `cmp-ok`, `isa-ok`, `does-ok`, `can-ok`, `dies-ok`, `lives-ok`, `eval-dies-ok`, `eval-lives-ok`, `throws-like`, `subtest`, `skip`, `todo`, `pass`, `flunk`, `bail-out`, `done-testing`, `diag`
- **`Grammar.rakudoc`** — Grammar type methods: `parse`, `parsefile`, `subparse`
- **`Match.rakudoc`** — Match object methods and structure
- **`Regex.rakudoc`** — Regex type documentation
- **`Str.rakudoc`** (57 KB) — String methods (comprehensive)
- **`List.rakudoc`** (54 KB) — List methods
- **`Any.rakudoc`** (57 KB) — Any type methods (inherited by most types)
- **`Cool.rakudoc`** (52 KB) — Cool type coercion methods
- **`Hash.rakudoc`** — Hash methods
- **`Array.rakudoc`** — Array methods
- **`Int.rakudoc`**, **`Num.rakudoc`**, **`Rat.rakudoc`**, **`Complex.rakudoc`** — Numeric type methods
- **`Range.rakudoc`** — Range type methods
- **`Junction.rakudoc`** — Junction type (any, all, one, none)
- **`IO/Path.rakudoc`** and related `IO/` types — File I/O methods
- **`independent-routines.rakudoc`** (57 KB) — Built-in functions not tied to a specific type (e.g., `say`, `print`, `put`, `note`, `dd`, `exit`, `sleep`, `elems`, `keys`, `values`, etc.)

## How to use

- When implementing a new operator or fixing precedence: read `Language/operators.rakudoc`
- When implementing regex features: read `Language/regexes.rakudoc`
- When implementing grammar support: read `Language/grammars.rakudoc` and `Type/Grammar.rakudoc`
- When implementing Test module functions: read `Type/Test.rakudoc`
- When implementing methods on a type: read the corresponding `Type/<TypeName>.rakudoc`
- When parser behavior is unclear: read `Language/syntax.rakudoc` and `Language/traps.rakudoc`
