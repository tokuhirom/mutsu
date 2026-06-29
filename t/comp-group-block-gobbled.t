use Test;

# A brace block gobbled by a comma list / list-op in a `for` iterable produces
# an X::Comp::Group bundling an X::Syntax::BlockGobbled sorrow with the
# X::Syntax::Missing (block) panic.
# https://github.com/Raku/old-issue-tracker/issues/1076

throws-like 'for 1,2,3, { say 3 }', X::Comp::Group,
    sorrows => sub (@s) { @s[0] ~~ X::Syntax::BlockGobbled && @s[0].message ~~ /^Expression/ },
    panic => sub ($p) { $p ~~ X::Syntax::Missing && $p.what ~~ /^block/ };

# A plain missing block (no gobbled block) is still the bare X::Syntax::Missing,
# not a group.
throws-like 'for 1, 2 say 3', X::Syntax::Missing, what => 'block';

# Normal for-loops with a block keep working.
my @seen;
for 1, 2, 3 { @seen.push($_) }
is @seen, [1, 2, 3], 'for loop with block still runs';

for (1, 2, 3) { }
pass 'parenthesised iterable + block parses';

done-testing;
