use Test;

plan 14;

# A quoted literal inside a `< ... >` assertion may contain the angle brackets
# themselves. Neither the regex-literal scanner nor the assertion parser may
# count those toward the `<`/`>` nesting depth.

ok  ('ab'  ~~ / <!before '%>' > . /),  'a quoted ">" does not close the assertion';
ok  ('%>a' ~~ / <?before '%>' > . /),  'and the literal still matches';
ok  ('>a'  ~~ / <?before '>' > . /),   'a bare quoted ">" works too';
ok  ('ab'  ~~ / <!before '<%' > . /),  'a quoted "<" does not open a nested assertion';
ok  ('<%a' ~~ / <?before '<%' > . /),  'and that literal matches';
ok  ('ab'  ~~ / <!before "%>" > . /),  'double quotes behave the same';

# The same inside a group, and with an alternation in the assertion.
ok  ('ab'  ~~ / [ <!before '%>' > . ]* /), 'inside a bracketed group';
ok  ('ab'  ~~ / <!before '<%' || \n > . /), 'with an alternation next to the literal';

# The escaped forms that already worked must keep working.
ok  ('ab'  ~~ / <!before \> > . /),    'an escaped ">" still works';
nok ('>a'  ~~ / ^ <!before \> > . /),  'and still rejects a real ">"';

# A quote character in a NON-lookaround `< ... >` is a literal, not a string
# opener — treating it as one would swallow the rest of the regex.
ok  ("b" ~~ / < a ' b > /),            'a quote in a word-list alternation stays literal';
ok  ("'" ~~ / <[ ' ]> /),              'and in a character class';

# A grammar token is parsed by the same scanner.
grammar G {
    token TOP { <chunk>+ }
    token chunk { '<%' <-[%]>* '%>' | [ <!before '<%' || \n > . ]+ }
}
ok  G.parse('hello'),          'a grammar token with a quoted "<" parses and matches';
ok  G.parse('a<%x%>b'),        'and handles the delimiter it guards against';
