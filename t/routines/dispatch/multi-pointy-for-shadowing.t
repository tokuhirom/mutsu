use Test;

# A sibling multi candidate may capture the same name from the enclosing
# routine, but a pointy `for` parameter in the running candidate shadows it.
# This is the shape found in CSS::Properties' !box-value.
plan 2;

sub outer($prop) {
    multi sub S(Int $x) {
        my @result;
        for <a b> -> $prop {
            @result.push: $prop;
        }
        @result.join(',');
    }
    multi sub S(Str $x) { $prop }
    S(1);
}

is outer('P'), 'a,b', 'the pointy loop parameter shadows the sibling capture';
is outer('Q'), 'a,b', 'the shadowing remains correct across another activation';
