use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# The mechanisms overload::constant relies on (#15804): a parametric actions role
# `Numish[%handlers]` whose `numish` reads the Match by key (`integer`,
# `decimal-number`), calls a handler of the importing unit with the literal's
# text, and hands the result back as a `RakuAST::Constant`; the refinement ends
# with the block holding the `use`.

plan 9;

sub integer { "i$^a" }
sub decimal { "d$^a" }

is 1, 1, 'before the use: untouched';
{
    use OverloadConstantLike &integer, &decimal;
    is 42, 'i42', 'an Int literal goes to the integer handler';
    is 3.5, 'd3.5', 'a decimal literal goes to the decimal handler';
    is (7, 8).join(','), 'i7,i8', 'list elements too';
    is 1e3, 'd1e3', 'the handler gets the text as written (scientific)';
    is 0x2A, 'i0x2A', 'the handler gets the text as written (radix prefix)';
    is 1_000, 'i1_000', 'the handler gets the text as written (underscores)';
}
is 9, 9, 'after the block: untouched';

is RakuAST::Constant.new(42).value, 42, 'RakuAST::Constant.value';
