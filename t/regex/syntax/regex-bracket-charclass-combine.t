use Test;

# An enumerated character class may combine a leading bracket class with named
# builtin classes: `<[a..z] +digit>`. Previously a bracket class followed by a
# `+name`/`-name` part matched nothing (it fell through both the pure-bracket
# and the pure-combined parsing branches).

plan 10;

ok "3" ~~ / ^ <[a..z] +digit> $ /, 'bracket + digit matches a digit';
ok "a" ~~ / ^ <[a..z] +digit> $ /, 'bracket + digit matches a letter';
nok "!" ~~ / ^ <[a..z] +digit> $ /, 'bracket + digit rejects punctuation';

ok "_" ~~ / ^ <[\-._~] +alpha> $ /, 'bracket with escapes + alpha matches underscore-set member';
ok "x" ~~ / ^ <[\-._~] +alpha> $ /, 'bracket with escapes + alpha matches a letter';

# Subtraction tail.
ok  "b" ~~ / ^ <[a..z] -[aeiou]> $ /, 'bracket minus bracket keeps a consonant';
nok "a" ~~ / ^ <[a..z] -[aeiou]> $ /, 'bracket minus bracket removes a vowel';

# Nested: a grammar token used as a char class that is itself a combined class.
grammar G {
    token TOP        { <reg-name> }
    token reg-name   { <+unreserved +sub-delims>* }
    token unreserved { <[\-._~] +uri-alphanum> }
    token sub-delims { <[;!$&'()*+,=]> }
    token uri-alphanum { <+alpha +digit> }
}
my $m = G.parse("ab-c.com");
ok $m.defined, 'nested combined char-class grammar matches';
is ~$m, 'ab-c.com', 'nested combined char-class captured whole host';

ok G.parse("a1-b_2").defined, 'nested combined class accepts digits and unreserved marks';
