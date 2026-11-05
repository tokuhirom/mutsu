# Regression tests: binding errors name the class; `is built(:bind)` keeps an Array un-itemized

Found by the raku-doc differential sweep (`Type/Associative.rakudoc`, `Type/Attribute.rakudoc`).
Both behaviours landed on `main` from a parallel change while this was in review; this entry
adds the sweep's two cases as tests identical under `raku`.
