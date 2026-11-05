use lib 't/lib';
use Test;
use CapturedRwMulti::Pick;

# Found via String::Fields: an imported `proto` whose multis include an
# `is rw` candidate cached the first call's winner by argument TYPE, so a
# literal passed after a variable picked the rw candidate and died with
# "expects a writable container".

plan 6;

my $v = "abc";
is pick($v, 1), 'rw', 'a variable picks the rw candidate';
is pick("abc", 1), 'plain', 'a literal after it picks the plain candidate';
is $v.&pick(1), 'rw', 'method-call form with a variable';
is "abc".&pick(1), 'plain', 'method-call form with a literal';
is pick($v, 1), 'rw', 'the variable still picks rw afterwards';
is pick("abc", 1), 'plain', 'and the literal still picks plain';
