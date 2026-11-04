unit module RwArgImported;

# From the TOML distribution: parse-value($t, $pos) hands its own `is rw`
# parameter on to parse-integer($t, $pos, :$zero-ok), whose `$pos++` must reach
# the caller's variable.
sub leaf(str $t, int $pos is rw, Bool :$zero-ok = False) { $pos++; 5 }
sub middle(str $t, int $pos is rw) is export { leaf($t, $pos) }
sub named-leaf(str $t, $pos is rw, :$z) is export { $pos++; 5 }
