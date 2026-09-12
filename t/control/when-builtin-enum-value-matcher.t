use v6;
use Test;

plan 8;

# A bareword `when` matcher that names a built-in enum VALUE is a complete
# nullary term, so the block after it belongs to the `when`. The parser's
# known-term list was a second, hand-written copy of the type registry's
# built-in enums and had drifted: `SeekType` and `Signal` are registered as
# real enums but were missing from it, so `given $whence { when
# SeekFromBeginning { ... } }` read the bareword as a listop call that gobbled
# the block and the whole compilation unit failed with "Function
# 'SeekFromBeginning' needs arguments". That is the shape IO::String's `seek`
# uses. Every assertion below was checked against rakudo itself.

sub which-seek($whence) {
    do given $whence {
        when SeekFromBeginning { 'begin' }
        when SeekFromCurrent   { 'current' }
        when SeekFromEnd       { 'end' }
        default                { 'other' }
    }
}

is which-seek(SeekFromBeginning), 'begin', 'SeekFromBeginning matches as a `when` bareword';
is which-seek(SeekFromCurrent), 'current', 'SeekFromCurrent matches';
is which-seek(SeekFromEnd), 'end', 'SeekFromEnd matches';
is which-seek(42), 'other', 'a non-member falls through to default';

sub which-signal($s) {
    do given $s {
        when SIGINT  { 'int' }
        when SIGTERM { 'term' }
        default      { 'other' }
    }
}

is which-signal(SIGINT), 'int', 'a Signal value matches as a `when` bareword';
is which-signal(SIGTERM), 'term', 'and so does another';

# The enums that already worked must keep working.
is (do given More { when Less { 'less' }; when More { 'more' }; default { 'other' } }),
    'more', 'an Order value still matches';
is (do given BigEndian { when LittleEndian { 'le' }; when BigEndian { 'be' }; default { 'other' } }),
    'be', 'an Endian value still matches';

done-testing;
