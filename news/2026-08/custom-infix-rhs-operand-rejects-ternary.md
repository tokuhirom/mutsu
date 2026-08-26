# A trait-less custom infix now has additive precedence, so a following ternary parses

`sub infix:<amic>($m, $n) { $m == $n }; say 2 amic @pair[1]??" yes"!!"no";`
(from `Language/experimental.rakudoc`'s amicable-numbers example) failed to parse
in mutsu with `Expected a term, but found either infix ?? or redundant prefix ?`.

## Root cause

Not, as the ticket guessed, a narrower operand parser. mutsu parsed a
user-declared `infix:<word>` at the **list-infix** precedence level — looser than
`??`/`!!`, `&&`, `||`, `..`, `~` and the comparisons. The `say` listop's argument
therefore ended at `2 amic @pair[1]`, and the leftover `??" yes"!!"no"` was
handed to the statement parser as a fresh statement, where a bare `??` in term
position is `X::Syntax::DuplicatedPrefix`.

Rakudo gives a trait-less `sub infix:<...>` **additive** precedence. Probing
`raku` v2026.06 pins it exactly: `1 zz 2 * 3` is `1 zz (2*3)`, `1 zz 2 ~ 3` is
`(1 zz 2) ~ 3`, and `1 zz 2 + 3` is `(1 zz 2) + 3`. So the operator is strictly
between `*` and `~` — additive — and every one of mutsu's looser-than-that
bindings (`~`, `..`, `&&`, `??`) was wrong, not just the ternary one.

Interestingly `additive_expr` already *documented* additive as "the default for a
trait-less infix" and honoured it for **symbol** operators under the
longest-token rule; only the word form fell through to the list-infix layer.

## Fix

The custom-infix-word application — operand, `is assoc` folding, trailing
colonpair adverbs, the `right`/`list`/`non`/`chain` fold — was extracted from the
list-infix loop into `try_custom_infix_word`
(`src/parser/expr/precedence/custom_infix.rs`), so one implementation now serves
both levels. `additive_expr` calls it for the default level; the list-infix loop
keeps calling it only for operators explicitly pushed down there with `is
looser`.

The subtlety that made the first attempt blow up: `parse_custom_infix_word` is
deliberately **permissive** — it accepts any non-reserved word, because an infix
can also be installed at runtime (`my &infix:<same-in-Int> = ...`) with nothing
for the parser to consult. That speculative match is harmless at the loosest,
last-resort level, but at additive precedence it swallowed every ordinary
bareword following a term: `42 but Str` became `infix:<but>` and the GC soundness
smoke test started failing with "Two terms in a row". So the default level is now
granted only to an operator the parser has actually seen declared
(`is_user_defined_infix`); an undeclared speculative word keeps its historical
last-resort level. This is the same permissive-parse hazard recorded earlier for
word-form custom infixes, now bounded.

Pinned by `t/custom-operator-and-term-parsing.t` section 3, which checks the
default operator against `*`, `~`, `&&`, `??` and its own associativity, both
`is tighter` and `is looser`, `is assoc<right>`, the doc's amicable-numbers line,
and that `but` is still the mixin operator.
