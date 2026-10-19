use Test;

# A `use` in a block installs the constants it imports in the block's
# lexical pad (ADR-8751), as it does variables: there the import hides an
# outer constant of the same name, a closure taken in the block keeps it,
# and a routine written outside the block does not see it, nor an imported
# type. Checked against rakudo.

plan 10;

use lib 't/lib';

constant UIBC-TERM = 'outer-term';
my $uibc-sig = 'outer-sig';
class UibcType { method who { 'outer type' } }
sub outer-view { UIBC-TERM ~ ' ' ~ $uibc-sig ~ ' ' ~ UibcType.who }

my &kept;
{
    use UseInBlockConstants;
    is UIBC-TERM, 'module-term', 'the import hides an outer constant in the block';
    is $uibc-sig, 'module-sig', 'and an outer variable with a sigiled constant';
    is UibcType.who, 'module type', 'and an outer type';
    is outer-view(), 'outer-term outer-sig outer type',
        'a routine written outside the block sees none of them';
    &kept = -> { UIBC-TERM ~ ' ' ~ $uibc-sig };
    dies-ok { $uibc-sig = 1 }, 'a sigiled constant stays readonly';
}
is kept(), 'module-term module-sig', 'a closure taken in the block keeps the imports';
is outer-view(), 'outer-term outer-sig outer type', 'the outer names are untouched after the block';

{
    use UseInBlockConstants :my;
    is UIBC-MY, 'module-my', 'a tag imports a my-scoped constant';
}

sub in-routine($n) { use UseInBlockConstants; UIBC-TERM ~ "/$n" }
is (1..2).map({ in-routine($_) }).join(','), 'module-term/1,module-term/2',
    'a routine body imports on every call';
is UIBC-TERM, 'outer-term', 'and leaves the outer constant alone';
