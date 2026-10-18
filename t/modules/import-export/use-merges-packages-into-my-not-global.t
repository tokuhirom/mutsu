use Test;
use lib 't/lib';

plan 18;

# A `use`d module's packages are installed lexically in the importing
# scope (`MY::`), never in `GLOBAL` (#13802, ADR-8751).

class ProgramOwn { }

is GLOBAL::<StashMergeUnit>:exists, False, 'GLOBAL:: has no module package before a block use';
{
    use StashMergeUnit;
    is GLOBAL::<StashMergeUnit>:exists, False, 'GLOBAL:: has no module package inside the block';
    is MY::<StashMergeUnit>:exists, True, "the block's MY:: lists the merged module package";
    is MY::<Foo>:exists, True, "the block's MY:: lists the imported class";
    ok MY::.keys.grep('StashMergeUnit'), "the block's MY::.keys lists it";
    {
        is MY::<StashMergeUnit>:exists, False, "a nested block's own MY:: does not";
    }
}
is GLOBAL::<StashMergeUnit>:exists, False, 'GLOBAL:: has no module package after the block';
is MY::<StashMergeUnit>:exists, False, "the unit's MY:: does not list a block's merge";

{
    need StashMergeUnit;
    is MY::<StashMergeUnit>:exists, True, "a block's need merges into its MY::";
}

sub with-use { use StashMergePlain; MY::<StashMergePlain>:exists }
sub without-use { MY::<StashMergePlain>:exists }
is with-use(), True, "a routine body's use merges into its MY::";
is without-use(), False, "another routine's MY:: does not see it";

use StashMergePlain;
is MY::<StashMergePlain>:exists, True,
    "a top-level use of a package-less module lists the package it declares";
is GLOBAL::<StashMergePlain>:exists, False, 'GLOBAL:: does not';
is GLOBAL::<&stash-merge-plain-f>:exists, False,
    "GLOBAL:: has no package-less module's exported our sub";
is GLOBAL::<EXPORT>:exists, False, 'GLOBAL:: has no EXPORT only a module exported into';
is GLOBAL::.keys.grep(/StashMerge/).elems, 0, 'GLOBAL::.keys lists no module package';
is GLOBAL::<ProgramOwn>:exists, True, "the program's own class stays in GLOBAL::";
is StashMergePlain::Thing.hi, 'thing', 'the merged package still resolves';
