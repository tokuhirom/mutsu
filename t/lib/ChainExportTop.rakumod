# Three deep, with a hook at every level: Top -> Outer -> Inner.
use ChainExportOuter;

sub EXPORT() {
    Map.new: ('&chain-top' => &chain-outer)
}
