use v6;
use Test;

# PDF::ISO_32000_2 indexes its class type object through a user-defined
# AT-POS; this is positional dispatch, not type parameterization.
plan 2;

class IndexedType {
    method table($index) is also<AT-KEY> { $index }
    method AT-POS(Int $index) { self.AT-KEY($index) + 1 }
}

is IndexedType.[41], 42,
    'a type object with AT-POS dispatches positional indexing to that method';
is IndexedType<catalog>, 'catalog',
    'a type object with AT-KEY dispatches angle indexing to that method';
