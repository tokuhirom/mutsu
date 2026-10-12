use Test;

# A leading `:my $n …;` is evaluated before the rest of the pattern is
# parsed. A later bare `$n` must still read the variable when the cursor
# reaches it -- after a code block assigned it -- not the value it had when
# the match started (#13004).

plan 9;

ok "x a" ~~ / :my $n; { $n = "x" } $n .* /,
    'a :my without an initializer, assigned in a code block, interpolates its new value';
ok "x" ~~ / :my $n = "q"; { $n = "x" } $n /,
    'a code block assignment replaces the initializer before the interpolation';
nok "y" ~~ / :my $n = ""; { $n = "x" } $n /,
    'the interpolation does not see the stale empty initializer';
ok "x a" ~~ / :my $n = ""; $n .* /, 'an initializer alone still interpolates';
ok "ab" ~~ / :my $n; { $n = "b" } a $n /, 'the interpolation follows other atoms';

my $r = "b";
ok "ab" ~~ / :my $n; { $n = $r } a $n /, 'the code block can read an outer lexical';
ok "x" ~~ / :my $n = "q"; <?{ $n = "x"; True }> $n /,
    'an assertion that assigns the variable is seen too';

grammar Indented {
    token TOP { :my $ind; <?before (\s*) { $ind = ~$0 }> $ind 'item' }
}
ok Indented.parse("  item"), 'a grammar token reads the indent its lookahead measured';
nok Indented.parse("item "), 'and still fails when the text does not match';
