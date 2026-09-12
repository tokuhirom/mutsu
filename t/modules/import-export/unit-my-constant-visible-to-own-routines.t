use v6;
use Test;
use lib 't/lib';
use UnitMyConstant;

# A `unit module`'s file-scope `constant` must be readable from that module's
# own routines, for every sigil and for both the `my constant` and bare
# `constant` spellings.
#
# mutsu treated the two spellings differently: a `constant` parses as
# `VarDecl { is_our: true }` but `my constant` parses with `is_our: false`, and
# the compunit-lexical collector discriminated on `is_our` rather than on the
# `__constant` trait. So a `my constant` was moved into `unit_lexicals` and
# thereby REMOVED from the module-scope store its own routines read, and every
# such read answered `Nil`.
#
# Found via the Business::CreditCard distribution, whose `cardtype` reads four
# `my constant` lookup tables (`@lookup`, `@renamed`, `%CUPcountry`,
# `%JCBcountry`) and so classified every card as `NotACreditCard`.

plan 14;

is-deeply my-array(), (10, 20, 30), 'my constant @array read from own routine';
is my-array().elems, 3, 'my constant @array has its elements';
is-deeply my-hash().Hash, {a => 1, b => 2}, 'my constant %hash read from own routine';
is my-hash().elems, 2, 'my constant %hash has its elements';
is my-scalar(), 99, 'my constant $scalar read from own routine';
is my-bare(), 'sigilless', 'my constant sigilless read from own routine';

is-deeply bare-array(), (40, 50, 60), 'bare constant @array read from own routine';
is bare-array().elems, 3, 'bare constant @array has its elements';
is-deeply bare-hash().Hash, {c => 3}, 'bare constant %hash read from own routine';
is bare-scalar(), 77, 'bare constant $scalar read from own routine';

# The importer does NOT see a unit compunit's file-scope constants bare
# (#7787) — the fix must not have made them leak into the loading scope.
nok ::('@MY-ARRAY') ~~ Positional, 'my constant @array does not leak to importer';
nok ::('MY-BARE').defined, 'my constant sigilless does not leak to importer';
nok ::('@BARE-ARRAY') ~~ Positional, 'bare constant @array does not leak to importer';
nok ::('$BARE-SCALAR').defined, 'bare constant $scalar does not leak to importer';
