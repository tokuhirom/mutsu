module BlockModuleMyTypeObject {
    # The braced form keeps its lexicals in the module body; it must keep
    # reading `Int`/`Any` and must not leak these names to the importer.
    my Int $block-typed;
    my $block-untyped;

    sub read-block-typed() is export { $block-typed }
    sub read-block-untyped() is export { $block-untyped }
}
