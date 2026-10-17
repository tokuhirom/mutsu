use Test;

# The tail of `dispatch_method_by_name_3` is late rows of the receiver's MRO
# (ADR-11276 §9.139). Answers checked against Rakudo.

plan 17;

class Foo { method Str { 'FOO' } }

is [Foo.new, Foo.new].Str, 'FOO FOO', 'a list Str reads a user Str';
is (Foo.new, 1).Seq.Str, 'FOO 1', 'a Seq Str reads a user Str';
is [Foo.new].Stringy, 'FOO', 'a list Stringy reads a user Str';
is (gather { take Foo.new }).Stringy, 'FOO', 'a gather Stringy reads a user Str';
is Date.Date.^name, 'Date', 'Date.Date';
is DateTime.DateTime.^name, 'DateTime', 'DateTime.DateTime';
is (1, 2, 3).Seq.Numeric, 3, 'Seq.Numeric';
{
    my %h{Int} = 1 => 'a';
    is %h.keys.map(*.^name).List, ('Int',), 'an object hash keeps its key objects';
}
"ab" ~~ /(a)$<x>=(b)/;
ok $/.EXISTS-KEY('x'), 'Match.EXISTS-KEY of a named capture';
nok $/.EXISTS-KEY('y'), 'Match.EXISTS-KEY of a missing one';
ok $/.EXISTS-POS(0), 'Match.EXISTS-POS in range';
nok $/.EXISTS-POS(3), 'Match.EXISTS-POS out of range';
is $/.AT-KEY('x').Str, 'b', 'Match.AT-KEY';
is (a => 1).AT-KEY('a'), 1, 'Pair.AT-KEY';
is-deeply \(1, 2, :a(3)).values.List, (1, 2, 3), 'Capture.values';
dies-ok { Supply.grab }, 'grab on the Supply type object';
{
    my $t = Thread.new(code => { 1 });
    ok $t.run === $t, 'Thread.run is the thread itself';
    $t.finish;
}
