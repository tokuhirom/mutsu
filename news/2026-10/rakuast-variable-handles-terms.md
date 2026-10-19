# Variable-backed handles terms round-trip through RakuAST

Attribute and method `handles @names` terms now retain their variable node in
RakuAST and lower back to the existing delegation path. EVAL resolves the
variable's method names and forwards calls for both declarations.
