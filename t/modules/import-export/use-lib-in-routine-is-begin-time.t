# `use lib` changes the repository chain at BEGIN time wherever it is written:
# rakudo's chain already holds the path of a `use lib` inside a routine that is
# never called. The BEGIN-time preload of a `use` nested in a not-yet-run block
# (GH #8201, `compiler::begin_use`) replays the unit's literal `use lib` specs
# first, so it must find them in every position, not only on the unit's
# top-level statement list (ADR-0137).
use Test;

plan 2;

sub never-called { use lib 't/lib/nested-use-lib'; }

my &later = { use NestedUseLibFixture; };

is X::NestedUseLib::Marker.^name, 'NestedUseLibFixture::X::NestedUseLib::Marker',
   'a module found through a routine-nested `use lib` is preloaded for a nested `use`';

ok $*REPO.repo-chain.first(*.Str.contains('nested-use-lib')),
   'the routine-nested `use lib` is in the repository chain';
