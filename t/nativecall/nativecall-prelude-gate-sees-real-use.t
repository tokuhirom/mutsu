# The other half of GH #7611: making the NativeCall prelude gate blind to
# prose must not make it blind to a real `use`. A `use NativeCall` nested
# inside a block is still a use, and the prelude it gates -- the `Pointer`
# family of type objects and the exported helper routines -- has to be there.
#
# (The module-body case is pinned by `t/nativecall-helpers-are-not-reexported.t`,
# whose fixture is a `unit module` that uses NativeCall.)
use Test;

plan 3;

{
    use NativeCall;

    is Pointer.^name, 'NativeCall::Types::Pointer',
       'a block-scoped use NativeCall still brings in the Pointer type objects';
    is nativesizeof(int32), 4,
       'and its exported helper routines';
    ok defined(::('&nativecast')),
       'and nativecast is resolvable, as a real use asks for';
}
