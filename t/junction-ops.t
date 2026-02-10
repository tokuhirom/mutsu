use Test;
plan 7;

ok 1 | 2 == 2, 'junction | binds before comparison';
nok 1 | 2 == 3, 'junction | false when no match';

ok 1 & 1 == 1, 'junction & all matches';
nok 1 & 2 == 2, 'junction & fails when not all match';

ok 1 ^ 2 == 2, 'junction ^ one match';
nok 1 ^ 1 == 1, 'junction ^ fails when more than one matches';

ok (1 | 2).WHAT eq '(Junction)', 'junction operator constructs junction';
