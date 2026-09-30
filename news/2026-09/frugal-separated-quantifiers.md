# Frugal separated quantifiers

Separated regex quantifiers such as `a*? % ','` and `a**?2..3 % ','` now try the shortest admissible chain first, while still growing when following tokens require it. Outside `:s` sigspace mode, the parser preserves the frugal modifier, and both the walk and compiled regex engine match Rakudo's candidate order.
