# A module whose imports arrive through a `sub EXPORT` hook (as Red's do):
# the importing unit then parses every bareword as an imported term.
sub EXPORT {
    Map.new: ('&hooked' => sub { 'hooked' })
}
