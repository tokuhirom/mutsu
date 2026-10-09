# `:@!attr = Empty` in BUILD leaves an Array in the attribute

A `submethod BUILD(:@!ab = Empty)` bound the attribute to the raw `Empty` Slip (or the List for
`= ()`), so a later `self.ab = |@x` stored a Slip and `.map(*.ab)` flattened. The attributive
parameter binder now re-homes a Slip/List into an Array, as rakudo does. Found by the
Math::NIntegrate ecosystem run: `t/11`, `t/20` and `t/23` now pass.
