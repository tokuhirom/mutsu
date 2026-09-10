use Test;
use lib 't/lib';
use NCDerefTagMod;

# A handle produced by `Pointer[T].deref` must carry T's registered name, the
# same as one produced by `nativecast(T, …)`. Tagged with the short name it
# resolved neither its own class's hand-written methods nor raku's `.^name`.

plan 9;

for (deref-body(), 'Pointer[T].deref (type from a hash)'),
    (deref-body-literal(), 'Pointer[T].deref (literal type)'),
    (cast-body(), 'nativecast(T, ...)') -> ($handle, $label) {
    is $handle.^name, 'NCDerefTagMod::LexBody', "$label: .^name is the registered name";
    is $handle.a, 0, "$label: a generated accessor reads the struct";
    is $handle.sum, 0, "$label: a hand-written method resolves";
}
