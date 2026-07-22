use Test;

# A `{ ... }` embedded code block in a slash-delimited regex is Main-slang code:
# a `/` inside it (most often the `$/` match variable) must not terminate the
# regex. Regression: `/ (\d) { say $/ } \d+ /` used to fail to parse ("Confused.
# unparsed input" at the `}`) because the closing-delimiter scanner saw the `/`
# of `$/` inside the block.

plan 6;

lives-ok {
    'ab' ~~ / a { my $z = $/; } b /;
}, 'a $/ reference inside a regex code block parses';

{
    my $seen;
    '123' ~~ / (\d) { $seen = $/.Str } \d+ /;
    is $seen, '1', 'the code block observed $/ at the capture point';
    is ~$/, '123', 'the whole regex still matched to the end';
}

# A literal slash inside a string inside the block is also safe.
lives-ok {
    'x' ~~ / x { my $p = "a/b"; } /;
}, 'a slash in a string in a code block does not close the regex';

# Existing constructs still parse correctly.
ok 'aa' ~~ / a ** {2} /, 'a {n} quantifier still works';
ok 'abc' ~~ /abc$/, 'a trailing $ anchor still works';
