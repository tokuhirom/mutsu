use Test;

plan 28;

# A `:m` (ignoremark) rule called as `<r>` runs as a frame, entering its ends
# one at a time: a code block in the callee runs once per end entered, not once
# per end computed. Each expectation was checked against rakudo.

my @log;
grammar G {
    regex TOP { <r> 'x' }
    regex r { :m a+ { @log.push: $/.to } }
}
ok G.parse('aax'), 'a :m callee matches';
is @log.join(','), '2', '... and its code block ran only at the end that was entered';

@log = ();
ok G.parse('aax').<r>.Str eq 'aa', '... and captures the right span';
is @log.join(','), '2', '... again';

# Over a subject that has a mark the callee still matches mark-insensitively.
my $marked = "a\x[301]a" ~ "x";
ok G.parse($marked), 'a :m callee matches over a subject with a combining mark';
@log = ();
ok G.parse("aax"), 'and still over a plain subject afterwards';

# Over a subject that has marks the callee runs as a frame too: each atom
# compares the base of the grapheme it stands on and consumes the whole
# grapheme, so its code block still runs only at the end entered (#7548).
@log = ();
is ~G.parse("a\x[301]aax")<r>, "a\x[301]aa", 'marked subject: the callee spans the marked grapheme';
is @log.join(','), '3', '... and its code block ran only at the end entered';

grammar A { regex TOP { <r> 'x' }; regex r { :m <[ab]>+ { @log.push: $/.to } } }
@log = ();
ok A.parse("a\x[301]b\x[300]ax"), 'a character class compares bases';
is @log.join(','), '3', '... entering one end';

grammar B { regex TOP { <r> 'x' }; regex r { :m .+ { @log.push: $/.to } } }
@log = ();
is ~B.parse("a\x[301]bx")<r>, "a\x[301]b", '`.` consumes a whole marked grapheme';
is @log.join(','), '3,2', '... entering ends as it backtracks, as rakudo does';

grammar C { regex TOP { <r> 'x' }; regex r { :m :i A+ { @log.push: $/.to } } }
@log = ();
ok C.parse("a\x[301]Ax"), ':m with :i';
is @log.join(','), '2', '... entering one end';

grammar E { regex TOP { <r> 'x' }; regex r { :m ( a+ ) $<k>=[ b ]? } }
my $m = E.parse("a\x[301]ab\x[302]x");
is ~$m<r>[0], "a\x[301]a", 'a positional capture spans the marked graphemes';
is ~$m<r><k>, "b\x[302]", 'a named capture too';

grammar H { token TOP { <r> }; token r { :m 'café' } }
ok H.parse("cafe\x[301]") && H.parse("cafè") && H.parse("cafe"), 'a marked literal matches any marks on its base';

# An LTM `|` in a markless callee ranks its branches over the mark-stripped
# subject (#13176).
grammar L { regex TOP { <r> 'x' }; regex r { :m [a | b]+ { @log.push: $/.to } } }
@log = ();
is ~L.parse("a\x[301]b\x[300]x")<r>, "a\x[301]b\x[300]", 'an LTM alternation over marked graphemes';
is @log.join(','), '2', '... entering one end';
grammar M { token TOP { <r> }; token r { :m [ 'ab' | 'abc' ] } }
is ~M.parse("a\x[301]bc\x[302]")<r>, "a\x[301]bc\x[302]", 'the longest branch wins, measured on bases';

# A zero-width assertion in a markless callee decides on the bases: a word
# boundary does not fall between a letter and its mark.
grammar W2 { regex TOP { <r> }; regex r { :m << \w+ >> } }
is ~W2.parse("e\x[301]te\x[300]")<r>, "e\x[301]te\x[300]", '<< and >> around marked graphemes';
grammar W3 { regex TOP { <r> .* }; regex r { :m ^^ a <|w> } }
is ~W3.parse("a\x[301] b")<r>, "a\x[301]", '<|w> after a marked grapheme';

# A look-around in a markless callee runs its body markless over the same
# subject: it compares bases, and a lookbehind starts only on a grapheme
# boundary (#13176).
grammar K2 { regex TOP { <r> 'x' }; regex r { :m [a <!before b>]+ { @log.push: $/.to } a b } }
@log = ();
is ~K2.parse("a\x[301]a\x[300]ab\x[300]x")<r>, "a\x[301]a\x[300]ab\x[300]", 'a negative lookahead over marked graphemes';
is @log.join(','), '2', '... entering one end';
grammar K3 { regex TOP { <r> 'x' }; regex r { :m a+ <?after a> { @log.push: $/.to } } }
@log = ();
is ~K3.parse("a\x[301]a\x[300]x")<r>, "a\x[301]a\x[300]", 'a lookbehind over a marked grapheme';
is @log.join(','), '2', '... entering one end';
grammar K5 { regex TOP { <r> .* }; regex r { :m .+ <?before 'é'> { @log.push: $/.to } } }
@log = ();
is ~K5.parse("abe\x[300]e")<r>, "abe\x[300]", 'a lookahead body compares bases';
is @log.join(','), '3', '... entering one end';
