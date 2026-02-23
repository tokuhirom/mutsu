use Test;

plan 5;

grammar LTM {
    proto token TOP {*}
    token TOP:sym<a> { a**4 }
    token TOP:sym<c> { a**2 % c }
}

is ~LTM.subparse('aaaaaaaa'), 'aaaa', 'subparse uses proto token LTM';
is ~LTM.subparse('acacaca'), 'aca', 'subparse handles separator quantifier';
ok LTM.parse('aaaa').defined, 'parse succeeds on full match';
ok !LTM.parse('aaaab').defined, 'parse requires full input consumption';
ok !LTM.subparse('bbbb').defined, 'subparse returns Nil on no match';
