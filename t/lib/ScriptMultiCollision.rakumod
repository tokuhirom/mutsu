# A package-less module whose multi family shares its name with a family the
# importing script declares itself (#11081).
multi sub smc-family(Int $x) { "mod-int:$x" }
sub smc-call($x) is export { smc-family($x) }
sub smc-count is export { &smc-family.candidates.elems }
sub smc-block is export { -> $x { smc-family($x) } }
