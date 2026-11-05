use Test;

plan 5;

# A Slip argument is spliced into the junction constructor's arguments.
is none(1..42, Empty).raku, 'none(1..42)', 'Empty is not an eigenstate';
is any(1, Empty, 2).raku, 'any(1, 2)', 'Empty between values';
is any(1, slip(3, 4)).raku, 'any(1, 3, 4)', 'a non-empty Slip is spliced';
is one(Empty, 5).raku, 'one(5)', 'leading Empty';
is all(1, 2).raku, 'all(1, 2)', 'plain arguments are unchanged';
