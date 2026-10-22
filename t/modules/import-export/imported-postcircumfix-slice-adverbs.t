use lib 't/lib';
use Test;

plan 6;

# An imported `multi postcircumfix:<{ }>` is reached for the slice adverbs,
# which arrive as named arguments (#14501).
{
    use ImportedPostcircumfixAdverbs;
    my %m is PCM;
    is %m{"a"}, 'user key="a" adv={}', 'plain subscript';
    is %m{"a"}:k, 'user key="a" adv={:k}', ':k reaches the override';
    is %m{"a"}:p, 'user key="a" adv={:p}', ':p reaches the override';
    is %m{"a"}:v, 'user key="a" adv={:v}', ':v reaches the override';
    is %m{"a"}:kv, 'user key="a" adv={:kv}', ':kv reaches the override';
}

sub in-routine() {
    use ImportedPostcircumfixAdverbs;
    my %m is PCM;
    %m{"b"}:k
}
is in-routine(), 'user key="b" adv={:k}', 'in a routine body';
