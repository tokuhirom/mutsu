use Test;

# Str.index / Str.rindex take a Cool needle (or a list of them). A Regex is not
# Cool, so rakudo finds no candidate and dies instead of searching.

plan 8;

throws-like { "xabcx".index(/bc/) }, X::Multi::NoMatch,
    'index with a Regex needle dies';
throws-like { "xabcx".rindex(/bc/) }, X::Multi::NoMatch,
    'rindex with a Regex needle dies';
throws-like { "xabcx".index(/bc/, 1) }, X::Multi::NoMatch,
    'index with a Regex needle and a position dies';
throws-like { "xabcx".rindex(rx/bc/, 3) }, X::Multi::NoMatch,
    'rindex with a Regex needle and a position dies';

my $msg = do { try "xabcx".index(/bc/); ~$! };
like $msg, /'Cannot resolve caller index(Str:D: Regex:D)'/, 'the message names the call';
like $msg, /'none of these signatures matches'/, 'the message lists the candidates';

is "xabcx".index("bc"), 2, 'a plain needle still searches';
is "xabcx".rindex(<b c>), 3, 'a list of needles still searches';
