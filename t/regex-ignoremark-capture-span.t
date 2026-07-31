use v6;
use Test;

plan 8;

# ADR-0016 P3: captures under :m (ignoremark) derive their text from spans
# remapped into ORIGINAL subject space — raku reports the original (marked)
# text, not the mark-stripped text the engine matched against.
if "cafés" ~~ m:m/ caf (e) s / {
    is ~$0, "é", ':m capture reports original (marked) text';
    is $0.from, 3, ':m capture from is original-space';
    is $0.to, 4, ':m capture to is original-space';
}
else {
    flunk ':m match failed' for ^3;
}

# A subrule carrying its own :ignoremark: sub-captures must land at absolute
# subject positions (the engine matches a mark-stripped SLICE internally).
grammar IgnCap {
    token TOP { x <s> }
    token s   { :ignoremark '"' (\w+) '"' }
}
my $m = IgnCap.parse('x"abc"');
ok $m.defined, 'parse with :ignoremark subrule succeeds';
is ~$m<s>[0], 'abc', 'capture group inside :ignoremark subrule has right text';
is $m<s>[0].from, 2, '... and absolute from';
is $m<s>[0].to, 5, '... and absolute to';

# Marked text inside the ignoremark subrule: still original text out.
grammar IgnMark {
    token TOP { <w> }
    token w   { :ignoremark (ab) }
}
my $m2 = IgnMark.parse("áb");
is ~$m2<w>[0], "áb", 'marked chars survive into the capture text';
