use Test;

# The sequence operator binds more loosely than the comma and is list
# associative: `1 ... 3, 10 ... 20` is `1 ... (3, 10) ... 20`. A number or
# string endpoint ends its segment short and seeds the next; the last
# operand's values follow the last segment.

plan 13;

is ("A"..."C", "a"..."c", 0...2, "+").join(","), 'A,B,C,a,b,c,0,1,2,+', 'paren chain with string, number and trailing endpoints';
is (1...2, 5...6, 8...9).join(","), '1,2,5,6,8,9', 'three chained segments';
is (1...3, 10...20).join(","), '1,2,3,10,17,20', 'a segment that overshoots keeps its endpoint';
is (0...2, 5...4).join(","), '0,1,2,4', 'a segment past its endpoint produces nothing';
is (1,2 ... 4, 10 ... 20).join(","), '1,2,3,4,10,16,20', 'seeds before the first operator';
is (1 ... (3,10) ... 20).join(","), '1,2,3,10,17,20', 'directly nested chain';
is infix:<...>(1, (2,5), (6,8), 9).join(","), '1,2,5,6,8,9', 'n-ary infix:<...>';
is infix:<...>(1, (* >= 3, 10), 20).join(","), '1,2,3,10,11,12,13,14,15,16,17,18,19,20', 'a closure endpoint is consumed';
is infix:<...>(1, (5,7), 4).join(","), '1,2,3,4,4', 'trailing endpoint values follow the last segment';
is (1/4, 1/2, 1 ... 8, 9 ... *)[^10].join(","), '0.25,0.5,1,2,4,8,9,10,11,12', 'an infinite last segment stays lazy';
is do { my @a = "A"..."C", "a"..."c", 0...2, "+"; @a.join(",") }, 'A,B,C,a,b,c,0,1,2,+', 'bare chain in an assignment';
is do { my @d = 0...2, 5; @d.join(",") }, '0,1,2,5', 'bare single sequence with a trailing value';
is ( "A"..."Z", "a"..."z", 0...9, "+", "/" ).elems, '64', 'the base64 alphabet (HTTP::Tiny)';
