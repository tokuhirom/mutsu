use Test;

# #13023: reading a lazy Seq one subscript at a time. A prefix pull grows
# the pulled prefix in place instead of copying it whole, and a single
# position is read without copying the Seq into a List, so the loop below is
# linear. (How far a subscript pulls a `.map` Seq is #13166.)

plan 7;

{
    my $s = (1..100).map(* * 2);
    is $s[0], 2, 'the first element';
    is $s[5], 12, 'a later element';
    is $s[0], 2, 'an earlier element again';
}

{
    my $s = 'abcdef'.comb;
    is $s[1], 'b', 'a prefix pull of a Str cursor';
}

{
    my $n = 5000;
    my $s = (1..$n).map(* + 1);
    my $t = 0;
    $t += $s[$_] for ^$n;
    is $t, ($n * ($n + 1) div 2) + $n, 'every element, one subscript at a time';
    is $s.elems, $n, 'the Seq still holds all of them afterwards';
}

{
    my $s = (1, 2).map(* + 0);
    is-deeply $s[7], Nil, 'a position past the end is Nil';
}
