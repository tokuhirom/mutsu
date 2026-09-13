use Test;

# A `}` inside a string literal inside a regex code block is code, not the
# block's closing brace. Three brace-depth scans in `regex_parse_core.rs`
# counted braces without tracking quotes, so `{ say "a}b".chars }` was cut at
# the `}` inside the string: the code block became `{ say "a`, and the rest of
# the line was parsed as pattern text. The token then silently failed to match
# instead of running its block.
#
# Reported (tokuhirom/mutsu#8336) as needing a subrule argument round trip --
# `<inner("a}b")>` against a `token inner($x) { { say $x.chars } \w+ }`. It does
# not: the argument only mattered because the callee happened to hold a code
# block. The minimal repro has no parameter at all, and is the first case below.

plan 8;

# --- a bare code block in a token body ------------------------------------

{
    my $seen;
    grammar Dq { token TOP { { $seen = "a}b".chars } \w+ } }
    ok Dq.parse('zz'), 'a token whose code block holds a } in a string matches';
    is $seen, 3, '...and the block ran with the string intact';
}

{
    my $seen;
    grammar Sq { token TOP { { $seen = 'a}b'.chars } \w+ } }
    ok Sq.parse('zz'), 'the single-quoted spelling matches too';
    is $seen, 3, '...and its block ran';
}

# Only an unmatched CLOSER is covered. rakudo rejects an unmatched `{` inside
# such a string outright ("Two terms in a row"), so `"a{b"` in a regex code block
# has no reference behaviour to pin -- rakudo's own scan is not quote-aware for
# the opener either.

# --- the subrule-argument shape the ticket reported -----------------------

{
    my $seen;
    grammar Arg {
        token TOP       { <inner("a}b")> }
        token inner($x) { { $seen = $x.chars } \w+ }
    }
    ok Arg.parse('zz'), 'the reported `<inner("a}b")>` shape parses';
    is $seen, 3, '...and the argument arrives whole';
}

# --- the `** { code }` quantifier, which shares the same scan --------------

is ("aa" ~~ / a ** { 2 } /).Str, 'aa',
    'a `** { code }` quantifier still works';
is ("xx" ~~ / x ** { "a}b".chars - 1 } /).Str, 'xx',
    '...including one whose code holds a } in a string';
