use Test;
use lib 't/lib';

plan 2;

# The script loads the module first; the hook module's own `need` of it is
# then an already-loaded no-op, which must still make the module's qualified
# names (its EXPORT stash) visible from the hook module (#10683).
need NeedExportStash::Ops;
is NeedExportStash::Ops::EXPORT::DEFAULT.WHO<&plain-export>.(2), 'plain(2)',
    'a needed module exposes its exports through its EXPORT stash';

use NeedExportStash::Hook;
is 'c' ↱ 'd', 'p(c,d)', 'a hook re-exporting a module the script needed first';
