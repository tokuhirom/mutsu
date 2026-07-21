use Test;

plan 6;

grammar LTM {
    proto token TOP {*}
    token TOP:sym<a> { a**4 }
    token TOP:sym<c> { a**2 % c }
}

is ~LTM.subparse('aaaaaaaa'), 'aaaa', 'subparse uses proto token LTM';
is ~LTM.subparse('acacaca'), 'aca', 'subparse handles separator quantifier';
ok LTM.parse('aaaa').defined, 'parse succeeds on full match';
ok !LTM.parse('aaaab').defined, 'parse requires full input consumption';
# A failed .subparse yields a *failed* Match: it is defined but false, and
# gists as `#<failed match>` (unlike .parse, which returns a Failure/Nil).
my $failed = LTM.subparse('bbbb');
ok !$failed, 'subparse yields a false (failed) Match on no match';
is $failed.gist, '#<failed match>', 'failed subparse gists as #<failed match>';
