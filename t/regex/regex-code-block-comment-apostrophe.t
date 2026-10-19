use Test;

# The end of a regex code block is found by one scanner for every form
# (`{ … }`, `<?{ … }>`, `<{ … }>`). An apostrophe in a `#` comment or in an
# identifier opens no string literal there, so the block keeps its own
# closing brace (#13928).

plan 6;

{
    my @ran;
    ok 'ab' ~~ / . { @ran.push: 'block'; # Don't stop here
    } /, 'a block whose comment holds an apostrophe matches';
    is-deeply @ran, ['block'], '... and its code runs';
}

{
    my @ran;
    my sub don't { @ran.push: 'call' }
    ok 'ab' ~~ / . { don't() } /, 'a block calling an identifier with an apostrophe matches';
    is-deeply @ran, ['call'], '... and its code runs';
}

ok 'ab' ~~ / . <?{ # isn't false
    True }> /, 'a code assertion with an apostrophe in a comment';

grammar G {
    token TOP { :my $*seen = True; <t>+ { make $*seen } }
    token t {
        .
        {
            if True {
                # Don't change anything else.
            }
            $*seen = False;
        }
    }
}
is G.parse('ab').made, False, 'a rule code block with an apostrophe in a comment runs';

# vim: expandtab shiftwidth=4
