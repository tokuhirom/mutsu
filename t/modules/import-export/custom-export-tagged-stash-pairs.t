use Test;

# List::Util (zef:lizmat, 0.0.10) selects tagged exports with `:p` on its
# `EXPORT::<tag>` stash. Package stashes must support the same associative
# adverb path as ordinary hashes.

use lib 't/lib';
use CustomExportTaggedStash <selected>;

plan 1;

is selected(), 'selected',
    'custom EXPORT can select a tagged routine through stash :p lookup';
