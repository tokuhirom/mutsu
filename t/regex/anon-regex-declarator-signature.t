use Test;

plan 17;

# An anonymous `token`/`regex`/`rule` term may carry a signature, exactly like
# the named declarator: rakudo's `regex_def` is
# `<deflongname>? <signature>? '{' <p6regex> '}'`, with both optional and
# independent. The signature rides on the produced Regex value, so a subrule
# reference to the lexical it was stored in can bind it.
# https://github.com/tokuhirom/mutsu/issues/8293

my $tok = token ( $x ) { $x \d+ };
isa-ok $tok, Regex, 'token ( $x ) { ... } is a Regex';
isa-ok (regex ( $x ) { $x \d+ }), Regex, 'regex ( $x ) { ... } is a Regex';
isa-ok (rule ( $x ) { $x \d+ }), Regex, 'rule ( $x ) { ... } is a Regex';

# ... and the argument really binds: it is neither ignored nor mis-bound.
my &tk = token ( $x ) { $x \d+ };
ok  ('ab12' ~~ / <&tk: 'ab'> /),  'argument binds through <&name: arg>';
ok  ('ab12' ~~ / <&tk('ab')> /),  'argument binds through <&name(arg)>';
nok ('ab12' ~~ / <&tk: 'zz'> /),  'a non-matching argument really fails';
ok  ('ab12' ~~ / <&$tok('ab')> /), 'argument binds through <&$var(arg)>';

# An argument-less anonymous term is still reachable the same way.
my &plain = token { \d+ };
ok ('ab12' ~~ / <&plain> /), 'argument-less anonymous token via <&name>';

# The declarator's own flavour survives: `rule` injects the implicit `<.ws>`,
# and `token`/`rule` ratchet while `regex` does not.
my &spaced = rule ( $x ) { $x 'b' };
ok ('a b' ~~ / <&spaced: 'a'> /), 'anonymous rule gets implicit whitespace';
my &unspaced = token ( $x ) { $x 'b' };
nok ('a b' ~~ / <&unspaced: 'a'> /), 'anonymous token does not';

# The bound value reaches a `{ ... }` code block and a `<?{ ... }>` code
# assertion alike — both run at match time, in the caller's environment.
my &counted = token ( $n ) { \d ** {$n} };
is ('12345' ~~ / <&counted: 3> /).Str, '123', 'parameter reaches a code block';
my &asserted = token ( $x ) { <?{ $x eq 'wanted' }> \w+ };
ok  ('abc' ~~ / <&asserted: 'wanted'> /), 'parameter reaches a code assertion';
nok ('abc' ~~ / <&asserted: 'other'> /),  '... with the value actually passed';

# The same for a *named* declarator: a parameter was invisible inside
# `<?{ ... }>` there too.
grammar H {
    token TOP  { <inner('passed')> }
    token inner($x) { <?{ $x eq 'passed' }> \w+ }
}
ok H.parse('abc'), 'named token parameter reaches its code assertion';
grammar I {
    token TOP  { <inner('other')> }
    token inner($x) { <?{ $x eq 'passed' }> \w+ }
}
nok I.parse('abc'), '... and a wrong value fails the assertion';

# A regex is a closure over the scope its literal was written in; an anonymous
# declarator returned from a sub reads that scope from its code assertion.
sub make-token() {
    my sub upper($s) { $s.uc }
    my $helper = &upper;
    return token ( $text ) { <?{ $helper($text) eq 'BAR' }> \w+ };
}
my &closed = make-token();
ok  ('anything' ~~ / <&closed: 'bar'> /), 'anonymous token keeps its defining scope';
nok ('anything' ~~ / <&closed: 'baz'> /), '... and still honours its argument';
