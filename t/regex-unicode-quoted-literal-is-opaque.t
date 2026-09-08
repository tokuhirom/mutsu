use Test;

plan 23;

# A quoted literal's contents are DATA. The structural scanners that walk a
# pattern looking for regex operators skipped `'...'` and `"..."` but not the
# Unicode quote pairs, so a `%`, `|`, `&`, `<` or `>` inside one of those was
# mistaken for the separated-quantifier operator, an alternation, a conjunction
# or an assertion bracket. Every Unicode pair closes with a character that
# differs from its opener, which is why the old `in_single`/`in_double` boolean
# pairs could not express it -- a bool cannot say which delimiter ends the span.

# --- the separated-quantifier operator ---------------------------------------

ok 'a%b' ~~ / ^ ‘a%b’ $ /,   'a % inside a smart-quoted literal is a literal %';
ok 'a%b' ~~ / ^ “a%b” $ /,   'the same in double smart quotes';
ok 'a%b' ~~ / ^ ｢a%b｣ $ /,   'the same in Q-lang corner brackets';
ok 'a%b' ~~ / ^ 'a%b' $ /,   'the plain-quote form, which always worked';
ok '%'   ~~ / ^ ‘%’ $ /,     'a lone % is a literal';
ok 'a%%b' ~~ / ^ ‘a%%b’ $ /, 'a doubled %% is a literal too';

# The real separator quantifier must keep working beside it.
ok 'a,b,c' ~~ / ^ [ \w+ ]+ % ‘,’ $ /,
    'a smart-quoted separator still parses AS a separator';
ok 'a,b,c' ~~ / ^ [ \w+ ]+ % ',' $ /,
    'and so does the plain-quoted one';

# --- alternation and conjunction ---------------------------------------------

ok 'a|b'  ~~ / ^ ‘a|b’ $ /,  'a | inside a smart-quoted literal is a literal |';
ok 'a||b' ~~ / ^ ‘a||b’ $ /, 'and so is a doubled ||';
ok 'a&b'  ~~ / ^ ‘a&b’ $ /,  'an & inside one is a literal &';
ok 'a'    ~~ / ^ [ ‘a|b’ | ‘a’ ] $ /,
    'a real alternation beside a quoted | still splits correctly';

# --- angle brackets, in and out of an assertion ------------------------------

ok '<%' ~~ / ^ ‘<%’ $ /,     'an angle bracket inside a smart-quoted literal';
ok '%>' ~~ / ^ ‘%>’ $ /,     'and the closing one';
ok 'a'  ~~ / <!before ‘<%’> . /,
    'a smart-quoted literal carrying < inside a lookahead assertion';
ok 'a'  ~~ / <!before ‘%>’> . /,
    'and one carrying > -- the assertion must not close early';
ok 'a'  ~~ / <!before ｢<%｣> . /, 'the corner-bracket form of the same';
nok '<%' ~~ / ^ <!before ‘<%’> . /,
    'the assertion still actually fires when it should';

# --- the shape Template::Classic is built from -------------------------------

{
    my grammar G {
        token TOP  { ^ [ $<part> = <text> || $<part> = <code> || <!before $> { die 'Unterminated' } ]* $ }
        token text { [ <!before ‘<%’> . ]+ }
        token code { ‘<%’ [ $<put> = ‘=’ ]? $<source> = [ <!before ‘%>’> . ]* ‘%>’ }
    }
    my $m = G.parse('<ul><% x %></ul>');
    ok $m.defined, 'the Template::Classic grammar parses';
    is $m<part>.elems, 3, 'and finds all three parts';
    is $m<part>[0].Str, '<ul>', 'the leading text part';
    is $m<part>[1].Str, '<% x %>', 'the code part';
    is $m<part>[2].Str, '</ul>', 'the trailing text part';
}
