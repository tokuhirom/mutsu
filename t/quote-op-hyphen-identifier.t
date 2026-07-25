use v6;
use Test;

plan 12;

# A raku identifier may contain `-` (and `'`) when an alphabetic follows, and
# that beats the quoting constructs: `m-meta-ok` is a call to the routine
# `m-meta-ok`, never `m` with a `-` delimiter. HTTP::UserAgent's `t/001-meta`
# declares `my &m-meta-ok`.

sub m-meta-ok { "m" }
sub s-a-b { "s" }
sub q-a-b { "q" }
sub qq-a-b { "qq" }
sub Q-a-b { "Q" }
sub tr-a-b-c { "tr" }
sub rx-a-b { "rx" }

is m-meta-ok(), "m", 'm-meta-ok is a routine call';
is s-a-b(), "s", 's-a-b is a routine call';
is q-a-b(), "q", 'q-a-b is a routine call';
is qq-a-b(), "qq", 'qq-a-b is a routine call';
is Q-a-b(), "Q", 'Q-a-b is a routine call';
is tr-a-b-c(), "tr", 'tr-a-b-c is a routine call';
is rx-a-b(), "rx", 'rx-a-b is a routine call';

# A `-` NOT followed by an alphabetic cannot continue an identifier, so it is
# still a usable delimiter.
is ~("x1x" ~~ m-1-), "1", 'm-1- is still a match with a - delimiter';
is q-1-, "1", 'q-1- is still a q-string';

# The ordinary delimiters keep working.
is ~("xax" ~~ m/a/), "a", 'm// still works';
is q/hi/, "hi", 'q// still works';
my $t = "abc";
$t ~~ tr/a/z/;
is $t, "zbc", 'tr/// still works';
