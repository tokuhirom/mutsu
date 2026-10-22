use v6;
use Test;
use lib 't/lib';

plan 4;

# A sub of a `module M { }` block that an importer calls by its imported name
# reaches the routines the unit's top-level `use` imported, as it does when it
# is called as `M::name`.
use ModuleBlockImportedCmp;

module M {
    sub via-file-level($x) is export { imported-cmp($x, 2) }
}

module N {
    use ModuleBlockImportedCmp;
    sub via-block-use($x) is export { imported-cmp($x, 2) }
}

module Q {
    our sub qualified($x) { imported-cmp($x, 2) }
}

is Q::qualified(1), Less, 'called qualified';

import M;
import N;
is via-file-level(3), More, 'imported by name, through the unit\'s own use';
is via-block-use(1), Less, 'imported by name, through the module block\'s own use';
is via-file-level(2), Same, 'and a second call';
