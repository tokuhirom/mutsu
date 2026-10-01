use Test;
use lib 't/lib';

# ADR-0134 §2.1.1 (#10336): every top-level `use` and `constant` is a BEGIN-time
# effect. It runs in the unit's prologue, in source order, ahead of the run-time
# statements that precede it, and it sees lexicals in their static state.

plan 8;

my $seen = $*BEGIN-TIME-LOAD-FIXTURE;
use BeginTimeLoadFixture;
is $seen, 'loaded', 'a use loads its module before the run-time statements ahead of it';
is fixture-answer(), 42, '... and imports its symbols';

my $x = 5;
constant K = $x;
is K.raku, 'Any', 'a constant does not see a run-time initializer that precedes it';

my $y;
BEGIN $y = 3;
constant L = $y + 1;
is L, 4, 'a constant sees what an earlier BEGIN stored';

# A group declaration with a run-time initializer ahead of a `use` keeps its
# initializer.
my ($p, $q) = 1, 2;
use BeginTimeLoadFixture;
is "$p $q", '1 2', 'a destructuring declaration ahead of a use is intact';

# A block's `use` shadows an import of the same name from outside the block;
# leaving the block brings the outer one back.
use ExportHookTermVsTaggedSub :t;
{
    use ExportHookTermVsTaggedSub;
    is t.hi, 'hi', 'the block import installs the EXPORT term';
}
is t.hi, 'hi', 'the outer import of the same name is back after the block';

# A lexical pragma stays in its source position.
no strict;
$not-yet-strict = 1;
use strict;
is $GLOBAL::not-yet-strict // $not-yet-strict, 1,
    'use strict does not apply to the statements before it';
