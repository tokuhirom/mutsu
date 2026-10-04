# The regex tree walk is no longer reached over `t/` and the roast whitelist

ADR-0135 replaces the regex tree walk with a compiled backtracking engine. This change
covers the last places a match still reached the walk, measured over all of `t/` and the roast
whitelist (7,879 files):

- **LTM ranking.** The prefix measurement used the walk in two places: a user-defined `<ws>` at
  the start of a rule, and a character class that holds a grammar token, such as
  `<+alpha -[q]>` in a grammar that defines `alpha`. Both are now measured by the rule's own NFA.
- **`:m` without a subject.** A `:m` pattern matched outside a published subject now builds the
  subject from its characters.
- **Empty quantifier ranges.** `a ** 3..1` compiles to an op that raises "Quantifier range is
  empty", as before, and the separated form `a ** 3..1 % ','` now raises too. `a ** 0 % ','`
  compiles to its zero-iteration arm.

Calls that the growing-seed loop evaluates up front, mostly left recursion, run compiled
programs. They are now counted on a `regex-eager:` stats line of their own, not as uses of the
walk.

After this change, the only remaining uses of the walk come from tests that turn the compiled
engine off on purpose with `MUTSU_RX_VM=0`. ADR-0135's deletion criterion (D7) is met, and
deleting the walk is the next step. This is ADR-0135 §8, Slice E, part twenty-one.
