# An allomorph with a role mixin keeps the role in its `.WHAT`

`<42> but R` now reports `.WHAT` as the composed `IntStr+{R}` type object
instead of the bare `IntStr`, so `.^mro` and `.^mro(:roles)` start with
`(IntStr+{R})` exactly as Rakudo does and `.^mro[0] === .WHAT` holds. The
allomorph composition goes through the same composition-keyed shared type
object as every other role mixin (ADR-0060), layered on the allomorph type
rather than the numeric payload, so `.^set_name` on the value and on its
`.WHAT` agree too (#10296).
