use Test;

# `s:c(N)///` / `s:p(N)///` (and `S///`) start the substitution at N; `:p`
# anchors the first match there (#18892).

plan 12;

{ my $s = "abcabc"; $s ~~ s:c(2)/b/X/; is $s, 'abcaXc', 's:c skips an earlier match' }
{ my $s = "abcabc"; $s ~~ s:g:c(2)/b/X/; is $s, 'abcaXc', 's:g:c replaces only from the start' }
{ my $s = "abcabc"; $s ~~ s:continue(1)/b/X/; is $s, 'aXcabc', 'the long adverb name' }
{ my $s = "abcabc"; $s ~~ s:p(1)/b/X/; is $s, 'aXcabc', 's:p at a match' }
{ my $s = "abcabc"; $s ~~ s:p(0)/b/X/; is $s, 'abcabc', 's:p with no match at the position' }
{ my $s = "abcabc"; $s ~~ s:g:p(1)/b/X/; is $s, 'aXcaXc', 's:g:p anchors only the first match' }
{ my $n = 3; my $s = "abcabc"; $s ~~ s:c($n)/b/X/; is $s, 'abcaXc', 'an expression argument' }
is (S:c(2)/b/X/ given "abcabc"), 'abcaXc', 'S///';
{ my $s = "abc"; $s ~~ s:c(-1)/b/X/; is $s, 'abc', 'a negative start matches nothing' }
{ my $s = "abc"; $s ~~ s:c(10)/b/X/; is $s, 'abc', 'a start past the end matches nothing' }
{ my $s = "aXbXc"; $s ~~ s:continue(2):nth(1)/X/-/; is $s, 'aXb-c', ':c with :nth' }
{ my $s = "abcabc"; is ($s ~~ s:c(2)/b/X/).from, 4, '$/ is the match the start found' }
