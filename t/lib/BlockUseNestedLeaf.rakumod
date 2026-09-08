unit module BlockUseNestedLeaf;

# A plain exporting module, used to pin the OTHER direction: a block-scoped
# `use` of it must not leak its export past the block
# (`roast/S11-modules/lexical.t`'s rule). See
# `t/block-use-keeps-nested-module-imports.t`.
sub leaf-probe() is export { 'leaf' }
