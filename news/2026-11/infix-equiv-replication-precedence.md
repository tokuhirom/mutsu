# `is equiv(&infix:<x>)` / `xx` gives an operator the replication level

The precedence of a user operator declared `is equiv(&infix:<x>)` (or `xx`) was unknown to the
parser, so it fell back to the additive level and `$root ⪪ 1 + 2` read as `($root ⪪ 1) + 2`
(`Cannot resolve caller Numeric(...)`). `x` / `xx` now resolve to a replication level between
the additive and concatenation ones, as in rakudo. Qwiratry's navigation operators
(`⪪ ⪫ ⥷ σ ↱ ...`) are declared this way.
