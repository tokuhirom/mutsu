use Test;

plan 5;

# A regex holding only internal modifiers is not null: it matches the empty string.
is ("x" ~~ / :i /).Str, "", '/ :i / matches the empty string';
ok ("x" ~~ / :i :m /).Bool, '/ :i :m / matches';
is ("x" ~~ / :i /).from, 0, 'the empty match is at position 0';

# A truly empty regex is still a null regex.
throws-like { EVAL 'say "x" ~~ / /' }, X::Syntax::Regex::NullRegex, '/ / is still null';
throws-like { EVAL 'say "x" ~~ / a | /' }, X::Syntax::Regex::NullRegex, '/ a | / is still null';
