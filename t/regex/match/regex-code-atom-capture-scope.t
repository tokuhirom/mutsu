use Test;

# A code atom (`{ … }`, `<?{ … }>`, `:my …;`) belongs to the regex that contains
# it. Inside a `[ … ]` group, a `||` / `|` branch or a quantified group it sees
# that regex's captures and `$/` spans from the regex's own start; only a
# capturing `( … )` opens a scope of its own (its `$/` starts at the group and
# its `$0` is the group's first capture). The tree walk used to give a
# non-capturing group a scope of its own, so `$/.Str` started at the group and
# `$0` of the enclosing regex was invisible. Expected values are raku's.

plan 14;

my @seen;

"abc" ~~ / a [ b { @seen.push($/.Str) } ] c /;
is @seen.join('|'), 'ab', '$/ in a non-capturing group starts at the regex start';

@seen = ();
"abc" ~~ / a ( b { @seen.push($/.Str) } ) c /;
is @seen.join('|'), 'b', '$/ in a capturing group starts at the group';

@seen = ();
"abc" ~~ / (a) [ b { @seen.push($0.Str) } ] c /;
is @seen.join('|'), 'a', '$0 in a non-capturing group is the enclosing regex\'s';

@seen = ();
"abc" ~~ / (a) [ (b) { @seen.push($0.Str ~ $1.Str) } ] c /;
is @seen.join('|'), 'ab', 'a capture inside the group continues the enclosing numbering';

@seen = ();
"abc" ~~ / (a) ( (b) { @seen.push($0.Str) } ) c /;
is @seen.join('|'), 'b', 'a capturing group numbers its own captures from zero';

@seen = ();
"aaab" ~~ / [ a { @seen.push($/.Str) } ]+ b /;
is @seen.join('|'), 'a|aa|aaa', '$/ in a quantified group keeps the regex start';

@seen = ();
"abd" ~~ / a [ c { @seen.push('c') } || b { @seen.push($/.Str) } ] d /;
is @seen.join('|'), 'ab', 'a `||` branch sees the regex start';

@seen = ();
"abd" ~~ / a [ b <?{ @seen.push($/.Str); True }> | c ] d /;
is @seen.join('|'), 'ab', 'a `|` branch sees the regex start';

# How often each code atom runs, and in which order: once per cursor position
# reached, including after backtracking.
my @log;
ok so "aab" ~~ / a+ { @log.push("n" ~ $/.chars) } b /, 'a block between a loop and its continuation';
is @log.join(','), 'n2', 'the block runs once for the loop\'s first (longest) end';

@log = ();
nok so "aax" ~~ / a+ { @log.push("n" ~ $/.chars) } b /, 'the continuation rejects every end';
is @log.join(','), 'n2,n1,n1', 'the block runs at each end, in backtrack order, then again from the next start';

@log = ();
ok so "abc" ~~ / :my $x = 3; a { @log.push("x=$x") } b <?{ $x == 3 }> c /, ':my value is visible to later code';
is @log.join(','), 'x=3', 'the declaration ran before the block';
