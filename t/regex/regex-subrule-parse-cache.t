use Test;

# Regression test: a token/subrule with many alternations, called once per
# input character over a long string, must not be pathologically slow. Before
# the parse-result cache, `parse_regex` re-parsed the (large) token pattern on
# every match step, making this take tens of seconds and time out under prove.
# See roast/S05-metasyntax/longest-alternative.t (WithHugeToken block).

plan 4;

{
    my grammar Letters {
        token TOP { <ltr>+ }
        token ltr {
              'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j'
            | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't'
            | 'u' | 'v' | 'w' | 'x' | 'y' | 'z'
        }
    }

    ok Letters.parse('a' x 5000), 'many-alternation token matches a long string';
    is Letters.parse('hello').Str, 'hello', 'matches a normal word';
}

# Basic LTM still works (longest alternative wins).
{
    my $str = 'a' x 7;
    ok $str ~~ m/aa|a|aaaa/, 'alternation matches';
    is ~$/, 'aaaa', 'longest alternative wins regardless of order';
}
