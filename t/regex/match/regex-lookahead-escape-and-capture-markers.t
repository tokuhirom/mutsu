use v6;
use Test;

# Two regex-parsing fixes surfaced by zef's `Zef::Identity` REQUIRE grammar
# (`regex value { '<' ~ '>' [<( [[ <!before \>|\<|\\> . ]+?]* %% ['\\' . ]+ )>] }`).

plan 8;

# --- 1. An escaped delimiter inside a lookahead assertion ------------------
# `<!before \>>` is "not before a literal `>`": the escaped `\>` must be part of
# the assertion's inner pattern, not close the `<...>` early.
ok ('ab' ~~ / [ <!before \>> . ]+ /), 'escaped > inside <!before ...> parses and matches';
is ~$/, 'ab', 'the lookahead consumes up to (not including) a literal >';
ok ('a>b' ~~ / ^ [ <!before \>> . ]+ /) && ~$/ eq 'a',
    'the <!before \> > lookahead stops before a literal >';

# `<?after \<>` — an escaped `<` in a look-behind.
ok ('a<b' ~~ / . <?after \<> . /), 'escaped < inside <?after ...> parses';

# Alternation of escaped delimiters inside the lookahead.
ok ('xy' ~~ / [ <!before \>|\<|\\> . ]+ /), '<!before \>|\<|\\> alternation of escaped delimiters';

# --- 2. `<( ... )>` capture markers with complex content ------------------
# `<(` starts the capture-marker construct, NOT a subrule call `<name(args)>`.
# Bracket groups / `%%` separators / quotes inside must not be mis-read as a
# subrule argument expression.
{
    ok ('a.b.c' ~~ / x* <( [\w]+ %% '.' )> /), '<( [\w]+ %% "." )> capture markers parse';
    is ~$/, 'a.b.c', 'the capture markers restrict the match to the inner pattern';
}
{
    # A real subrule call INSIDE the capture markers is still instantiated.
    grammar G { token part { \w+ };  regex v { <( <part> )> } }
    is ~G.subparse('foo', :rule<v>), 'foo',
        'a subrule call inside <( ... )> still works';
}
