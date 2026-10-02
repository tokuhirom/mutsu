use Test;
use lib 't/lib';

plan 5;

# `need` loads a module without importing it, but the module's EXPORT stash
# is still populated: a `sub EXPORT` hook can re-export a `need`ed module's
# routines through `Mod::EXPORT::DEFAULT.WHO` (#10683).
use NeedExportStash::Hook;
is 'a' ↱ 'J', 'p(a,J)', 'an operator re-exported from a needed module dispatches to its candidate';
is plain-export(1), 'plain(1)', 'a plain sub re-exported from a needed module';

need NeedExportStash::Ops;
is NeedExportStash::Ops::EXPORT::DEFAULT.WHO.keys.sort.join(' '), '&infix:<↱> &plain-export',
    'the EXPORT::DEFAULT stash of a needed module lists its exports';
my &op = NeedExportStash::Ops::EXPORT::DEFAULT.WHO<&infix:<↱>>;
is op('x', 'y'), 'p(x,y)', 'the stash value is the module routine, not a by-name reference';
is &op.candidates.elems, 1, 'the stash value carries the multi candidate';
