use Test;

# A Match is Positional over its capture list, so a multi-dimensional
# subscript walks the captures instead of treating the Match as one scalar.
# Found via the IO::Maildir distribution (`$/[*;*]».Str` in `flags`).

plan 4;

my regex mf { \:2 [\,|(P|R|S|T|D|F)|(<:Ll>)]* $ }
"x:2,PD" ~~ &mf;
is-deeply $/[*;*]».Str.List, ("P", "D"), 'nested quantified captures flatten';

"ab" ~~ /(a)(b)/;
is-deeply $/[*;*].List, (), 'flat captures have no captures of their own';

"ab" ~~ /[(a)|(x)]* /;
is $/[*;*].elems, 1, 'one quantified capture';
is $/[*;*][0].Str, 'a', 'its value';
