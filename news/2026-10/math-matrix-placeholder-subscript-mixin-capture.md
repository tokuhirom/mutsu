# Math::Matrix: placeholders in `[a;b]`, row-slice swaps, mixin captures in add_method closures

Working the `Math::Matrix` distribution (ecosystem roulette) fixed three interpreter gaps:

- A `$^placeholder` used only inside a multi-dimensional subscript (`map { @rows[$^i;$^i-$start] }, ^n`)
  now makes the enclosing block take that parameter; before, the block had arity 0 and every
  index read the same topic.
- `@a[$i, $r] = @a[$r, $i]` on a plain Array of Arrays no longer dies with `X::NotEnoughDimensions`;
  only a genuinely shaped array demands one index per dimension.
- A closure handed to `^add_method` that captures a mixin-wrapped variable (`$attr does Role`) no
  longer leaks its capture into a caller frame when one such method calls another through method
  dispatch. This was AttrX::Lazy's lazy accessors reading and storing each other's attribute.

Test files `021`, `030`, `031` and `040` of Math::Matrix now pass; `022` still needs
`.hash`/`.Slip` to route through a user-defined `.list` (#10501).
