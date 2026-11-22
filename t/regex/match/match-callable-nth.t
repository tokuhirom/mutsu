use Test;

# A Callable `:nth` for `.match` / `.subst` is called once the number of matches
# is known and stands for its answer: a WhateverCode gets the count, a bare
# block is called with none. A single-index answer gives `.match` a single
# Match (#18790).

plan 9;

is-deeply ~"aabbb".match(/<[abc]>/, :nth({2})), 'a', ':nth({2}) is the 2nd match';
isa-ok "aabbb".match(/<[abc]>/, :nth({2})), Match, 'and a single Match, not a list';
is ~"aabbb".match(/<[abc]>/, :nth(*-1)), 'b', ':nth(*-1) is the next-to-last';
is-deeply "ab".match(/./, :nth(*-5)), Nil, 'an answer past the matches selects nothing';
is "aabbb".subst(/<[abc]>/, '', :nth({2})), 'abbb', '.subst :nth({2})';
is "aabbb".subst(/<[abc]>/, '', :nth(*-1)), 'aabb', '.subst :nth(*-1)';
is "aabbb".subst(/<[abc]>/, '', :nd{2;}), 'abbb', 'the :nd{2;} spelling';
is-deeply "aabbb".match(/<[abc]>/, :nth(1, 3))».Str, ('a', 'b'), 'a list :nth is unchanged';
is ~"aabbb".match(/<[abc]>/, :nth(2)), 'a', 'an Int :nth is unchanged';
