use Test;
use lib 't/lib';

plan 3;

# BlockUseNestedOuter is a `unit module` whose body `use`s the bare-file
# NativeCall. NativeCall's own exports belong to NativeCall's compunit: a block
# that `use`s the outer module imports only the outer module's exports.
{
    use BlockUseNestedOuter;
    is MY::.keys.grep(/<[Oo]>uter|cglobal|nativecast|trait_mod|refresh/).sort.List, ('&outer-probe', 'BlockUseNestedOuter'),
        'block MY:: lists only what the block imported';
    ok MY::<&outer-probe>.defined, 'the outer module\'s own export is imported';
    nok MY::<&cglobal>.defined, 'the nested module\'s export is not re-exported';
}
