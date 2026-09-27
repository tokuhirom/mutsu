use Test;
use lib 't/lib';

# An operator imported by a `use` inside a block of a `unit module` is aliased
# under the module's package (`M::infix:<->`), not `GLOBAL::`. Leaving the
# block must still drop that alias; it used to survive, so every later `-` in
# the module (a WhateverCode `* - 1`, a `$buf[*-1]` subscript) dispatched to
# the imported modular candidate. Found through the EC dist's ed25519, which
# computes a constant inside `{ use FiniteField; ... }`.

use BlockImportModUser;

plan 4;

is BlockImportModUser::inner-result(), 5, 'the block itself sees the imported operator';
is BlockImportModUser::outer-minus(), -2, 'a sub after the block uses the core infix:<->';
is BlockImportModUser::whatever-minus(), 2, 'a WhateverCode after the block uses the core infix:<->';
is BlockImportModUser::buf-last(), 9, 'a from-end subscript after the block resolves correctly';
