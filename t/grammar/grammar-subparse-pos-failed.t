use v6;
use Test;

plan 14;

grammar RepeatChar {
    token start($character) { $character+ }
}

# A successful subparse (the token matches a prefix from position 0).
is ~RepeatChar.subparse('bbbabb', :rule('start'), :args(\('b'))), 'bbb',
    'subparse matches the leading run';

# A full .parse that does not consume the whole input fails (returns Nil-ish).
nok RepeatChar.parse('bbbabb', :rule('start'), :args(\('b'))).defined,
    'parse requires full consumption';

# A failed .subparse yields a *failed* Match, not a Failure.
my $failed = RepeatChar.subparse('bbbabb', :rule('start'), :args(\('a')));
is $failed.gist, '#<failed match>', 'failed subparse gists as #<failed match>';
is $failed.^name, 'Match', 'failed subparse is a Match';
ok $failed.defined, 'failed subparse is defined';
nok $failed.Bool, 'failed subparse is false';
nok ?$failed, 'failed subparse is falsy in boolean context';
is $failed.from, 0, 'failed subparse .from is 0';

# `:pos(N)` anchors the subparse to begin exactly at position N.
is ~RepeatChar.subparse('bbbabb', :rule('start'), :args(\('a')), :pos(3)), 'a',
    ':pos(3) matches the "a" at offset 3';

grammar Word {
    token TOP { \w+ }
}
is ~Word.subparse('abcdef', :pos(2)), 'cdef', ':pos anchors a plain token';
is Word.subparse('abcdef', :pos(2)).from, 2, ':pos match reports its .from';

# A total no-match subparse also yields a failed Match.
grammar Digits {
    token TOP { \d+ }
}
my $nd = Digits.subparse('abc');
is $nd.gist, '#<failed match>', 'no-match subparse is a failed Match';
nok $nd, 'no-match subparse is falsy';

# A .parse failure is still a Failure/undefined (unchanged behavior).
nok Digits.parse('abc').defined, 'parse still fails on no digits';

done-testing;
