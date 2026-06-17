use Test;

plan 6;

# A backslash directly followed by whitespace (`\ `) is an "unspace" — a main-slang
# construct that is not allowed inside a regex. Raku rejects it at compile time
# with X::Syntax::Regex::Unspace.

throws-like '/\ X/', X::Syntax::Regex::Unspace,
    'backslash-space in a regex throws Unspace';
throws-like '/\ X/', X::Syntax::Regex::Unspace,
    message => / 'No unspace allowed in regex' /,
    'Unspace message mentions the rule';

{
    my $err;
    try { EVAL '/\ X/'; CATCH { default { $err = $_ } } }
    is $err.char, ' ', 'the .char attribute is the offending whitespace';
}

# Legitimate forms that match a literal space still work.
{
    ok 'a b' ~~ / a \x20 b /, '\x20 matches a literal space';
    ok "a b" ~~ / a ' ' b /, "quoted ' ' matches a literal space";
    ok 'ab' ~~ / a b /, 'sigspace regex with real whitespace is unaffected';
}
