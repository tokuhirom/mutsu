use v6;
use Test;

# After a `{ }` code block, Rakudo has published the cursor as `$/`, so the
# argument list of a later `<.m(...)>` call in the same rule sees the text
# matched so far. A rule without a code block, and a caller whose callee ran
# the block, do not (#16308).
plan 4;

my @seen;
grammar WithBlock {
    token TOP { <d>+ }
    method h($s) { @seen.push: "[$s]"; self }
    token d { <[0..9]> { } <.h($/.Str)> }
}
WithBlock.parse('012');
is @seen.join(' '), '[0] [1] [2]', 'a code block makes the match so far visible to the call';

@seen = ();
grammar Growing {
    token TOP { <d> }
    method h($s) { @seen.push: "[$s]"; self }
    token d { <[0..9]> { } <[0..9]> <.h($/.Str)> <[0..9]> <.h($/.Str)> }
}
Growing.parse('0123');
is @seen.join(' '), '[01] [012]', 'each call sees the text up to itself';

@seen = ();
grammar AssertionBlock {
    token TOP { <[0..9]> <?{ True }> <.h($/.Str)> }
    method h($s) { @seen.push: "[$s]"; self }
}
AssertionBlock.parse('7');
is @seen.join(' '), '[7]', 'an assertion block counts as a code block';

@seen = ();
grammar CalleeBlock {
    token TOP { <d> <.h($/.Str)> }
    method h($s) { @seen.push: "[$s]"; self }
    token d { <[0..9]> { } }
}
CalleeBlock.parse('0');
is @seen.join(' '), '[]', 'a block in a callee does not publish the caller\'s $/';
