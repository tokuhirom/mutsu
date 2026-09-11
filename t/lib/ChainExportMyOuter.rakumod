# Same chain, but the outer hook is a `my sub` (Identity::Utils' shape). The
# two declarations need not share a scope kind to collide.
use ChainExportInner;

my sub EXPORT() {
    Map.new: ('&chain-my-outer' => &chain-inner)
}
