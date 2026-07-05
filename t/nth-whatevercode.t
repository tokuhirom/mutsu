use Test;

# :nth on .subst / .match accepts a Whatever (`*`, the last match) or a
# WhateverCode (`*-1`, `*-2`, ...) resolved against the match count. mutsu
# previously evaluated the Whatever eagerly to 0, throwing "Attempt to retrieve
# before :1st match".

plan 16;

# subst with Whatever / WhateverCode
is "a1b2c3".subst(/\d/, "X", :nth(*)), "a1b2cX", 'subst :nth(*) hits last match';
is "a1b2c3".subst(/\d/, "X", :nth(*-1)), "a1bXc3", 'subst :nth(*-1) is second-to-last';
is "aaaa".subst(/a/, "b", :nth(*-2)), "abaa", 'subst :nth(*-2)';
is "aaaaa".subst(/a/, "X", :nth(*-1)), "aaaXa", 'subst :nth(*-1) over five';

# match with Whatever / WhateverCode
is "a1b2c3".match(/\d/, :nth(*)).Str, "3", 'match :nth(*) is last';
is "a1b2c3".match(/\d/, :nth(*-1)).Str, "2", 'match :nth(*-1)';

# Out-of-range Whatever selects nothing (no error)
is "a1b2c3".match(/\d/, :nth(*-5)), Nil, 'match :nth(*-5) beyond range is Nil';
is "abc".subst(/x/, "Y", :nth(*)), "abc", 'subst :nth(*) with no matches is a no-op';

# Plain integer / range / list forms still work
is "a1b2c3".subst(/\d/, "X", :nth(1)), "aXb2c3", 'subst :nth(1)';
is "aaaa".subst(/a/, "b", :nth(2)), "abaa", 'subst :nth(2)';
is "aaaa".subst(/a/, "b", :nth(2..3)), "abba", 'subst :nth(2..3)';
is "aaaa".subst(/a/, "b", :nth(1,3)), "baba", 'subst :nth(1,3)';
is "aaaa".subst(/a/, "b", :2nd), "abaa", 'subst :2nd';
is "abcabc".match(/b/, :nth(2)).from, 4, 'match :nth(2)';
is "aaa".match(/a/, :nth(1..2)).elems, 2, 'match :nth(1..2)';

# s/// with WhateverCode nth
my $s = "a1b2c3";
$s ~~ s:nth(*)/\d/Z/;
is $s, "a1b2cZ", 's:nth(*)/// hits last match';
