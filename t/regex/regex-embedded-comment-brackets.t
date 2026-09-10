use v6;
use Test;

# `#`«...»` (and other bracket pairs) is a bracket-delimited *embedded* comment
# inside the regex/token slang: it ends at the matching close bracket and the
# pattern may continue on the same line. mutsu previously only mapped the ASCII
# bracket openers (`[ ( { <`), so a guillemet `#`«...»` never found its close
# and ate the rest of the line (including the token's `}`), desyncing the parse.
# Regression surfaced by Gzz::Text::Utils (grammar tokens with `#`« ... »` docs).

plan 9;

grammar G1 { token TOP { 'a' #`« a guillemet comment » 'b' } }
ok  G1.parse('ab'),  "guillemet embedded comment, pattern continues after it";
nok G1.parse('a'),   "the 'b' after the comment is still required";

grammar G2 { token TOP { 'x' #`[ square ] 'y' } }
ok G2.parse('xy'), "square-bracket embedded comment";

grammar G3 { token TOP { 'p' #`( round ) 'q' } }
ok G3.parse('pq'), "round-bracket embedded comment";

grammar G4 { token TOP { 'm' #`«« doubled »» 'n' } }
ok G4.parse('mn'), "doubled-guillemet embedded comment";

# Line comment (plain `#`) still runs to end of line.
grammar G5 {
    token TOP {
        'a' # a line comment to EOL
        'b'
    }
}
ok G5.parse('ab'), "plain # line comment still works";

# Embedded comment in an m// match, mid-pattern.
ok 'ab' ~~ / a #`{ brace comment } b /, "embedded comment in m// regex";

# A guillemet comment followed by more grammar declarations must not leak.
grammar G6 {
    token one { 'A' #`« doc » }
    token TOP { <one> 'Z' }
}
ok G6.parse('AZ'), "comment before a token's closing brace does not leak into the next token";

# Nested brackets inside the embedded comment are balanced (depth-counted).
grammar G7 { token TOP { 'a' #`[ outer [ inner ] still comment ] 'b' } }
ok G7.parse('ab'), "nested brackets inside an embedded comment are balanced";
