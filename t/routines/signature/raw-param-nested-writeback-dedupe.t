use Test;

# A sigilless parameter passed on through several multi candidates (the
# Array::Sorted::Util shape: `inserts` -> `inserts` -> `finds`/`insert`) keeps
# writing through to the caller's array, call after call.

plan 4;

my multi sub put-in(@a, $pos, \value) { @a.splice($pos, 0, value); $pos }

my multi sub find-pos(\haystack, $needle, :&cmp = &[cmp]) {
    my int $i = 0;
    my int $elems = haystack.elems;
    $i = $i + 1 while $i < $elems && cmp($needle, haystack.AT-POS($i)) === More;
    $i
}

my multi sub ins(\haystack, $needle, :&cmp = &[cmp]) {
    my $i := find-pos(haystack, $needle, :&cmp);
    put-in(haystack, $i, $needle);
    (my int $j = $i);
    $j = $j + 1 while $j < haystack.elems && haystack.AT-POS($j) == $needle;
    $j
}
my multi sub ins(\haystack, $needle, **@also, :&cmp = &[cmp]) {
    my $i := ins(haystack, $needle, :&cmp);
    put-in(@also[0], $i - 1, @also[1]);
    $i
}

my @k;
my @v;
ins(@k, $_, @v, "v$_") for 5, 1, 3, 3, 9, 1;
is-deeply @k, [1, 1, 3, 3, 5, 9], 'the keys are sorted in place';
is @v.elems, 6, 'every value was inserted beside its key';
is-deeply @v.grep(*.defined).elems, 6, 'no value was lost';
my &rev = -> $a, $b { $b cmp $a };
my @d;
ins(@d, $_, :cmp(&rev)) for 2, 7, 4;
is-deeply @d, [7, 4, 2], 'a passed comparator is used';
