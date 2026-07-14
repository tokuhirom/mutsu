use Test;

plan 7;

# A feed operator may open a continuation line. `==>` and `<==` are symbol operators
# that can never begin a statement, so crossing a newline is unambiguous.
# (The feed is looser than `=`, so the whole chain needs its own parentheses here --
# `my $x = (1..10) ==> ...` assigns the range and feeds afterwards, in raku too.)
my $forward = ((1..10)
    ==> grep * > 5
    ==> elems());
is $forward, 5, '==> continues across a newline';

my @backward;
@backward
    <== grep * > 5
    <== (1..10);
is-deeply @backward, [6, 7, 8, 9, 10], '<== continues across a newline';

is ((1..10) ==> grep * > 5 ==> elems()), 5, '==> on one line is unchanged';

# An endpoint-less closure sequence must be pulled on demand, not truncated to the
# 32-element prefix that `eval_sequence` generates up front.
my $count = 0;
for 2, 3, *+2 ... * {
    $count++;
    last if $_ > 7000;
}
is $count, 3501, 'for over an infinite closure sequence pulls past its eager prefix';

my @fib;
for 1, 1, *+* ... * {
    @fib.push($_);
    last if $_ > 100;
}
is @fib.tail, 144, 'a two-seed infinite closure sequence keeps generating';

# The loop must stay lazy: an infinite sequence is never materialized.
my $iterations = 0;
for 1, *+1 ... * {
    $iterations++;
    last if $iterations == 5000;
}
is $iterations, 5000, 'an infinite closure sequence stays lazy for 5000 pulls';

# A `return` out of such a loop still carries its value.
sub first-above($limit) {
    for 2, 3, *+2 ... * {
        return $_ if $_ > $limit;
    }
}
is first-above(1000), 1001, 'return out of an infinite closure-sequence loop works';
