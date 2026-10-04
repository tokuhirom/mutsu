use Test;
use nqp;

# `nqp::nativecallcast` to an `is repr('Uninstantiable')` type has nothing to
# box into and dies, as MoarVM's cast does. Upstream NativeCall's
# `Pointer.deref` on an untyped pointer casts to `void`, which is declared
# that way (#11203).

plan 2;

class V is repr<Uninstantiable> { }
class P is repr<CPointer> { }

dies-ok { nqp::nativecallcast(V, V, nqp::box_i(8, P)) },
    'a cast to an Uninstantiable type dies';
lives-ok { nqp::nativecallcast(P, P, nqp::box_i(8, P)) },
    'a cast to a CPointer type still works';
