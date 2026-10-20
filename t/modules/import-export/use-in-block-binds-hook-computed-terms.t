use Test;
use lib 't/lib';

# A term a `sub EXPORT` hook returns under a computed key (`%( $t => ... )`,
# the shape of Terminal::ANSI::OO) is kept in the importing block's lexical
# import set (ADR-8751, #14140): a closure taken in the block keeps it, and
# neither a routine written outside the block nor the code after it sees it.

plan 6;

sub outside { (try EVAL 't') // 'none' }

my &c = do { use ExportHookTermVsTaggedSub; -> { t.hi } };
is c(), 'hi', 'a closure taken in the block keeps the term';

{
    use ExportHookTermVsTaggedSub;
    is t.hi, 'hi', 'the block reads the term';
    isa-ok t, ExportHookTermVsTaggedSub, 'a bare t is the term';
    is EVAL('t.hi'), 'hi', 'an EVAL in the block finds it';
    is outside(), 'none', 'a routine written outside the block does not see it';
}

is (try EVAL 't') // 'gone', 'gone', 'the term is gone after the block';
