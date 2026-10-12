use Test;

# A `multi` family declared in a block is that block's `&name` binding for the
# closures nested in it (ADR-12529 phase 2, slice 6): a closure calling it by
# bare name keeps working after the block exited, and sees the candidates
# visible at the block, including an outer scope's.

plan 9;

{
    my &c;
    {
        multi q(Int) { 'q-int' }
        multi q(Str) { 'q-str' }
        &c = -> { q(5) ~ ' ' ~ q('x') };
    }
    is c(), 'q-int q-str', 'closure calls a block multi after the block exited';
}

{
    my &c;
    {
        multi q2(Int) { 'live-int' }
        multi q2(Str) { 'live-str' }
        &c = -> { q2(1) ~ ' ' ~ q2('a') };
        is c(), 'live-int live-str', 'the same closure while the block is live';
    }
}

multi r(Int) { 'r-int-outer' }
{
    my &c;
    {
        multi r(Str) { 'r-str-inner' }
        &c = -> { r(1) ~ ' ' ~ r('s') };
    }
    is c(), 'r-int-outer r-str-inner', 'the binding holds the outer candidates too';
}
{
    my $s = 's';
    is (try r($s)) // 'no Str candidate', 'no Str candidate',
        'the inner candidate stays out of the outer scope';
}

{
    my &c;
    {
        proto pq($) {*}
        multi pq(Int $x) { "int $x" }
        multi pq(Str $x) { "str $x" }
        &c = -> $v { pq($v) };
    }
    is c(3) ~ ' / ' ~ c('t'), 'int 3 / str t', 'a block proto with its candidates';
}

{
    my &c;
    {
        multi ns(Int $x) { "int " ~ callsame }
        multi ns(Cool $x) { 'cool' }
        &c = -> { ns(7) };
    }
    is c(), 'int cool', 'callsame from a candidate called through the binding';
}

{
    my &c;
    {
        multi sw(Int $x) { samewith(~$x) }
        multi sw(Str $x) { "str $x" }
        &c = -> { sw(9) };
    }
    is c(), 'str 9', 'samewith from a candidate called through the binding';
}

{
    my &c;
    {
        multi sib(Int) { 'sib-int' }
        multi sib(Str) { 'sib-str' }
        sub caller-sub { sib(1) ~ ' ' ~ sib('z') }
        &c = -> { caller-sub() };
    }
    is c(), 'sib-int sib-str', 'a block sub calls its sibling multi after exit';
}

{
    my &c;
    {
        multi nm(Int $x, :$tag = 'none') { "$x $tag" }
        multi nm(Str $x) { "str $x" }
        &c = -> { nm(1, :tag<t>) ~ ' / ' ~ nm(2) ~ ' / ' ~ nm('w') };
    }
    is c(), '1 t / 2 none / str w', 'named arguments through the binding';
}
