use lib 't/lib';
use Test;
use ImportedPostcircumfixAdverbs;

plan 2;

# The unit-level `use` form of imported-postcircumfix-slice-adverbs.t (#14501).
my %m is PCM;
is %m{"a"}:k, 'user key="a" adv={:k}', ':k reaches the override';
is %m{"a"}:p, 'user key="a" adv={:p}', ':p reaches the override';
