# The outer half: a module that BOTH `use`s a `sub EXPORT` module and declares
# its own hook -- the dominant lizmat "re-export a dependency under another
# name" idiom. Both hooks are live at once while this compunit loads, which is
# what used to collide on the single `GLOBAL::EXPORT` registry key (#7947).
use ChainExportInner;

sub EXPORT() {
    Map.new: ('&chain-outer' => &chain-inner)
}
