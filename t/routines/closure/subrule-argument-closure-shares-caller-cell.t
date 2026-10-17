use Test;

# A closure created while a grammar subrule's argument runs shares the
# caller's variable through a cell, whether it runs inside the argument or
# later, passed on as a rule argument. Nothing is copied back when a call
# returns (ADR-12529 §3, slice 46). Checked against rakudo.

plan 4;

{
    my $calls = 0;
    grammar G { token TOP { <x({ $calls++; 2 }())> }; token x($n) { a } }
    G.parse('a');
    is $calls, 1, 'a block called inside the argument';
}

{
    my $called = 0;
    grammar K {
        regex e($f) { \w+ <?{ $f() }> }
        token TOP { <e(-> { $called++; True })> }
    }
    ok K.parse('ab'), 'a block passed on as a rule argument';
    ok $called > 0, 'runs later and still writes the caller variable';
}

{
    my $n = 0;
    grammar D { token TOP { <x($n++)> }; token x($v) { a } }
    D.parse('a');
    is $n, 1, 'a direct write in an argument';
}
