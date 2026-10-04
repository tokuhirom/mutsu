use Test;

# An exception raised while smartmatching (a user `ACCEPTS` that dies) is
# raised by the construct that matched -- `~~`, `grep`, `first`, `when` --
# and never left behind for a later, unrelated match to raise.

plan 6;

class M { method ACCEPTS($) { die "accepts boom" } }

throws-like { 5 ~~ M.new }, X::AdHoc, message => 'accepts boom',
    '~~ raises the ACCEPTS exception';

throws-like { my @r = (1, 2).grep(M.new) }, X::AdHoc, message => 'accepts boom',
    'grep raises the ACCEPTS exception';

throws-like { (1, 2).first(M.new) }, X::AdHoc, message => 'accepts boom',
    'first raises the ACCEPTS exception';

throws-like { given 3 { when M.new { } } }, X::AdHoc, message => 'accepts boom',
    'when raises the ACCEPTS exception';

# A matcher that was consulted where the exception could not be raised must
# not leave it pending for the next match.
try { my @r = (1, 2).grep(M.new) };
lives-ok { my $ = 3 ~~ Int }, 'a later unrelated smartmatch does not raise it';
is (3 ~~ Int), True, 'and answers normally';
