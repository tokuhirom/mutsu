# Escaped operator names and exported proto aliases

`our &infix:["\c[DOUBLE PLUS]"] is export = &concat` now loads: the BEGIN-time resolved
name is also used when a unit ends in such a declaration, and the importer's parse-time
export scan spells the operator out, so `⧺` parses after `use`. A dispatcher captured
from an `our proto` and called from outside its module now goes through the qualified
family, so narrower candidates win over declaration order. Found through
List::Operator::DoublePlus (all 21 assertions of `t/all.rakutest` pass).
