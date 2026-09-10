use Test;

# A `{ ... }` embedded code block in a slash-delimited regex is Main-slang code:
# a `/` inside it (most often the `$/` match variable) must not terminate the
# regex. Regression: `/ (\d) { say $/ } \d+ /` used to fail to parse ("Confused.
# unparsed input" at the `}`) because the closing-delimiter scanner saw the `/`
# of `$/` inside the block.

plan 8;

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

# A scalar Range returned by a quantifier block is a range specification, not
# an exact numeric count. Scalar assignment itemizes the Range, so the
# quantifier evaluator must decontainerize it before dispatching on its kind.
my $quantifier-range = 1..3;
is ~('aaaa' ~~ / a ** {$quantifier-range} /), 'aaa', 'scalar Range bounds a greedy quantifier';
is ~('aaaa' ~~ / a **? {$quantifier-range} /), 'a', 'scalar Range bounds a frugal quantifier';
