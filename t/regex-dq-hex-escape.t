use Test;

# Bracketless \xHH (and \x[HH]) hex escapes inside a double-quoted string
# literal in regex slang. Regression for HTTP::Parser, whose grammar uses
# `token SP { "\x20" }` / `token HTAB { "\x09" }`.

plan 8;

ok ("a b" ~~ / "\x20" /),       'bracketless \x20 matches a space';
ok ("a\tb" ~~ / "\x09" /),      'bracketless \x09 matches a tab';
ok ("a\rb" ~~ / "\x0d" /),      'bracketless \x0d matches CR';
ok ("a  b" ~~ / "\x20\x20" /),  'two bracketless escapes in one string';
ok ("a b" ~~ / "\x[20]" /),     'bracketed \x[20] still matches';
ok ("xAy" ~~ / "\x41" /),       'bracketless \x41 matches A';

# The decoded character is exactly the codepoint (no stray "x"/digits left).
my $m = ("a b c" ~~ / (. "\x20" .) /);
is ~$m, 'a b', 'capture around \x20 spans exactly one space';

# In a grammar token used directly (no subrule indirection).
grammar G { token TOP { \w "\x20" \w } }
ok G.parse("a b"), 'grammar token with bracketless \x20 parses';
