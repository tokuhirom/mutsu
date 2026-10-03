use Test;

# A grammar method reached as a subrule (including a user `method ws` reached
# through sigspace) assigns to the caller's dynamic variables like any other
# method call. Only names the caller already binds are written back: a
# `my $*X` declared inside the method stays local, and lexicals stay
# isolated (#11326).

plan 7;

grammar H {
    token TOP { <foo> 'x' }
    token bar { <?> }
    method foo() { $*Z = 7; self.bar }
}
my $*Z = 0;
ok H.parse('x'), 'the grammar with a method subrule parses';
is $*Z, 7, 'a $*VAR assignment in a method subrule reaches the caller';

grammar Furthest {
    rule TOP { 'a' 'b' 'c' }
    method ws() {
        $*FURTHEST = self.pos if self.pos > $*FURTHEST;
        callsame;
    }
    method parse($target, |c) {
        my $*FURTHEST = 0;
        my $m = callsame;
        @*REPORT.push: $*FURTHEST;
        $m;
    }
}
my @*REPORT;
nok Furthest.parse('a b x'), 'a mismatch still fails';
is-deeply @*REPORT, [3], 'a `method ws` high-water mark is visible to the parse wrapper';

grammar L {
    token TOP { <foo> }
    token bar { <?> }
    method foo() { my $*Q = 9; $*Q = 10; self.bar }
}
my $*Q = 1;
ok L.parse(''), 'parses with a method-local dynamic';
is $*Q, 1, 'a `my $*Q` declared inside the method stays local';

grammar N {
    token TOP { <foo> }
    token bar { <?> }
    method foo() { $*NEW = 1; self.bar }
}
my $died = False;
try { N.parse(''); CATCH { default { $died = True } } }
ok $died, 'assigning an undeclared dynamic variable still dies';
