# The inner half of the nested-`sub EXPORT` chain (#7947): a module whose
# exports are computed by its own `sub EXPORT` hook.
sub chain-inner() { "inner" }

sub EXPORT() {
    Map.new: ('&chain-inner' => &chain-inner)
}
