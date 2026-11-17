use Test;

# A slice of an Array is a List of the Array's own element containers, so a
# method that hands the elements to a callback (or to the caller) writes
# through them (#17401).

plan 10;

{ my @c = 1, 2; @c[0..1].map({ $_ = 6 }).sink; is-deeply @c, [6, 6], '.map over a range slice writes through $_'; }
{ my @c = 1, 2, 3; @c[0, 2].map({ $_ *= 10 }).sink; is-deeply @c, [10, 2, 30], '.map over a list slice'; }
{ my @c = 1, 2; @c[0..1].grep({ $_ = 3; True }).sink; is-deeply @c, [3, 3], '.grep writes through $_'; }
{ my @c = 1, 2; @c[^2].first({ $_ = 4; False }); is-deeply @c, [4, 4], '.first writes through $_'; }
{ my @c = 1, 2, 3; for @c[1..2].kv -> $k, $v is rw { $v = 0 }; is-deeply @c, [1, 0, 0], '.kv values are the elements'; }

{
    my @c = 1, 2, 3;
    is-deeply @c[0..1].map(* * 2).List, (2, 4), 'a reading .map is unchanged';
    is @c[0..1].map({ $_ }).raku, '(1, 2).Seq', 'the elements render as values';
    my @copy = @c[0..1].map({ $_ });
    @copy[0] = 99;
    is-deeply @c, [1, 2, 3], 'assigning the result copies the values';
}
{ my @c = 1, 2; is-deeply @c[0..5].map({ $_ // "u" }).List, (1, 2, "u", "u", "u", "u"), 'a slice past the end reads holes'; }
{ is-deeply (1, 2)[0..1].map({ $_ }).List, (1, 2), 'a List slice is still a read'; }
