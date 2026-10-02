use v6;
use Test;

# From Deps: `for |$.factory-to: Type` where the callee returns a `lazy gather`
# -- `|` must flatten the lazy Seq, not hand the loop the Seq as one item.
plan 3;

sub a { lazy gather { take 1; take 2 } }
my @got;
for |a() { @got.push: .WHAT.^name }
is-deeply @got, ['Int', 'Int'], 'for |lazy gather iterates its values';

my $s = lazy gather { take 5 };
my $n = 0;
for |$s { $n += $_ }
is $n, 5, 'for |$lazy-seq';

my @inf;
for |(lazy gather { my $i = 0; loop { take $i++ } }) { @inf.push($_); last if $_ >= 3 }
is-deeply @inf, [0, 1, 2, 3], 'infinite lazy gather stays lazy';
