use Test;

plan 2;

grammar Layout {
    rule TOP {
        <!after '@'> # a comment between regex atoms
        <?before 'a'> 'a'
    }
}

ok Layout.parse('a'), 'rule comments preserve the following regex atoms';
ok !Layout.parse('@a'), 'lookbehind still rejects the excluded prefix';
