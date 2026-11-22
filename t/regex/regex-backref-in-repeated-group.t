use Test;

# A backreference inside a repeated group reads the capture of the iteration it
# sits in: in `[ (.) $0 ]+` each iteration's `$0` is that iteration's `(.)`
# (#18736, roast S05-mass/rx.t test 64). After the quantifier, `$0` is every
# iteration's capture in a row, as before.

plan 9;

is ~('bookkeeper' ~~ /[ (.) $0 ]+/), 'ookkee', '+ repeats doubled letters';
is ~('bookkeeper' ~~ /[ (.) $0 ]**2/), 'ookk', '** 2 takes two pairs';
is ~('bookkeeper' ~~ /[(.)$0]+/), 'ookkee', 'without spaces';
is ~('bookkeeper' ~~ /[ (.) [ $0 ] ]+/), 'ookkee', 'the backreference in a nested group';
is ~('bookkeeper' ~~ /[ (.) $0 | x ]+/), 'ookkee', 'in an alternation branch';
is ~('xaabbc' ~~ /[ (.) (.) $1 ]+/), 'xaa', 'the second capture of the iteration';
is ~('ookok' ~~ /^[ (.) $0 ]+/), 'oo', 'an earlier iteration does not satisfy it';
is ~('abab' ~~ /(.)+ $0/), 'abab', 'after the quantifier: every iteration in a row';
is ~('bookkeeper' ~~ /[ (.) $٠ ]+/), 'ookkee', 'an Nd-digit backreference';
