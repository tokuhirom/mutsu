# A slang actions role may carry helper methods

`use Test::Async::Decl` no longer fails slang activation with "grammar rule
override 'add_phaser' is not supported". Its actions role defines
`add_phaser`, a helper that the `package_declarator:sym<test-bundle>` and
`<test-reporter>` actions call as `self.add_phaser($/)`. Since ADR-0098, mutsu
had read every method of an actions role as a grammar-rule override, so the
helper was rejected. That also undid the Test::Async declarator support that
ADR-0091 had added.

ADR-13220 replaces that rule with Rakudo's own. An actions method is called
only for a production of the same name, so it is an override only when it
names one: a production the slang's own grammar declares, or one of Rakudo's
grammar. Every other method is a helper. Like every action body, it is never
run. An override of a production mutsu does not support is still a hard error
naming it, so the slang never silently parses under the wrong grammar.

Rakudo's production names come from a generated table,
`src/runtime/rakudo_grammar_productions.txt`. It holds the regex methods of
`$*LANG` under both the legacy and the RakuAST frontend, 1124 names on
Rakudo 2026.09. `scripts/gen-rakudo-grammar-productions.raku` regenerates it
from the `raku` oracle.

Test::Async itself still stops one step later. `use Test::Async` iterates
`Test::Async::Hub.HOW.bundles`, a method's `nqp::list` result, which mutsu
does not HLL-ize (#14290). The bundle registration that `add_phaser` stood in
for under precompilation needs a role-kind slang declarator to drive its
metaclass's `new_type` (#9926).
