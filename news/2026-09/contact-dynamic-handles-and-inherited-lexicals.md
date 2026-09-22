# Contact 0.0.5 is green

`Contact` 0.0.5 now loads and passes all five of its test files under mutsu,
matching Rakudo's 72 assertions. The distribution was selected by the
ecosystem roulette and locked on [mutsu #8977](https://github.com/tokuhirom/mutsu/issues/8977).

The fixes cover the general interpreter behavior the distribution exercises:

- capture slips in `handles` lists now expand expression results into delegated
  method names;
- nested class methods can read array and hash lexicals from enclosing package
  bodies, including inherited grammar regexes evaluated in a scratch
  interpreter; and
- mutable delegated dispatch preserves class-level attribute accessor
  precedence over a same-named method supplied by a role.

The regressions are pinned by `t/oo/trait/handles-dynamic-list.t`,
`t/oo/attribute/handles-dynamic-accessor-collision.t`, and
`t/modules/nested-class-package-array-lexical.t`.
