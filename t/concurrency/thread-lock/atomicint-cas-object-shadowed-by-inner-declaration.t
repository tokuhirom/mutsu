use Test;

# An object-valued scalar in a shadowing `my` has its own binding: a `cas`
# on it must not reach (or lose) the outer variable of the same name (#12107).

plan 3;

class A { has $.n }

{
    my $y = A.new(n => 1);
    my @seen;
    {
        my $y = A.new(n => 2);
        my $old = $y;
        cas $y, $old, A.new(n => 3);
        @seen.push: $y.n;
    }
    @seen.push: $y.n;
    is-deeply @seen, [3, 1], 'inner cas leaves the shadowed outer object alone';
}

{
    my $y = A.new(n => 1);
    my @seen;
    {
        my $o = $y;
        cas $y, $o, A.new(n => 10);
    }
    {
        my $y = A.new(n => 2);
        my $old = $y;
        cas $y, $old, A.new(n => 3);
        @seen.push: $y.n;
    }
    @seen.push: $y.n;
    is-deeply @seen, [3, 10], "outer's own cas survives an inner shadow";
}

{
    my $y = A.new(n => 1);
    my $z = $y;
    cas $y, $z, A.new(n => 5);
    is $y.n, 5, 'cas on a plain object scalar still swaps it';
}
