use Test;

# ADR-11276 slice 3C: Capture's views and its positional subscript are
# handler rows owned by Capture, and every collection's `.Capture` is one row
# per owner. Every answer below was checked against Rakudo.

plan 5;

my $capture = \(1, 2, a => 3);

subtest 'views interleave the positional and the named part', {
    plan 8;
    is $capture.keys.join(' '), '0 1 a', 'keys';
    is $capture.values.join(' '), '1 2 3', 'values';
    is $capture.kv.join(' '), '0 1 1 2 a 3', 'kv';
    is $capture.pairs.map({ .key ~ '=' ~ .value }).join(' '), '0=1 1=2 a=3', 'pairs';
    is $capture.antipairs.map({ .key ~ '=' ~ .value }).join(' '), '1=0 2=1 3=a', 'antipairs';
    is $capture.pairs.^name, 'Seq', 'a Seq';
    is $capture.list.join(' '), '1 2', 'list is the positional part';
    is $capture.hash.sort.map({ .key ~ '=' ~ .value }).join(' '), 'a=3', 'hash is the named part';
}

subtest 'sizes', {
    plan 4;
    is $capture.elems, 2, 'elems counts the positional part';
    is $capture.Numeric, 2, 'Numeric is the same';
    is \().elems, 0, 'an empty Capture';
    is \(:a(1)).keys.join(' '), 'a', 'a Capture with only a named part';
}

subtest 'positional subscript', {
    plan 8;
    is $capture.AT-POS(0), 1, 'AT-POS';
    is $capture.AT-POS(1), 2, 'AT-POS of the last one';
    is $capture.AT-POS(5).raku, 'Nil', 'AT-POS past the end is Nil';
    is $capture.AT-POS('1'), 2, 'a Str index is a number';
    is $capture.AT-POS(1.9), 2, 'a fractional index floors';
    isa-ok $capture.AT-POS(-1), Failure, 'a negative index is a Failure';
    ok $capture.EXISTS-POS(1), 'EXISTS-POS';
    nok $capture.EXISTS-POS(2), 'EXISTS-POS past the end';
}

subtest 'a negative EXISTS-POS is out of range', {
    plan 2;
    my $error;
    try { $capture.EXISTS-POS(-1); CATCH { default { $error = $_ } } }
    is $error.^name, 'X::OutOfRange', 'the exception class';
    is $error.message, 'Index out of range. Is: -1, should be in 0..^Inf', 'the message';
}

subtest 'Capture of the collections', {
    plan 7;
    is $capture.Capture.raku, $capture.raku, 'a Capture is its own Capture';
    is (1, 2).Capture.raku, '\(1, 2)', 'a List';
    is [1, 2].Capture.raku, '\(1, 2)', 'an Array';
    is (a => 1).Capture.raku, '\(:key("a"), :value(1))', 'a Pair';
    is bag(<a a b>).Capture.raku, '\(:a(2), :b(1))', 'a Bag';
    is %(a => 1, b => 2).Capture.raku, '\(:a(1), :b(2))', 'a Hash';
    is-deeply (class { method Str { 'foo' } } => 42,).Capture, \(:foo(42)),
        'a Pair key that is not a Str is named by its own .Str';
}
