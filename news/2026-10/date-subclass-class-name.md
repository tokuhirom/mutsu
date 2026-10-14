# Date/DateTime subclass results keep the subclass name

`succ`, `pred`, `raku`, `WHICH` and `utc` of an instance of `class MD is Date` or
`class MDT is DateTime` now answer under the subclass name (`MD.new(2024,3,1)`,
`ValueObjAt.new("MD|60369")`) instead of the base class. The row handlers take the receiver's
class name (#12994).
