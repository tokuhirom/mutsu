# A multi EXPORT hook must be scoped just like an ordinary sub EXPORT. The
# arity-suffixed registry key must not shadow the inner module's hook.
use ChainExportInner;

multi EXPORT(+@args) {
    Map.new: ('&chain-multi-outer' => &chain-inner)
}
