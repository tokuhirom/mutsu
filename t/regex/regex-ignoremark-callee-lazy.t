use Test;

plan 17;

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
