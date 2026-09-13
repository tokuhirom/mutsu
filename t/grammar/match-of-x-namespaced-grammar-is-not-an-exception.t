use Test;

# A grammar's Match is typed by the grammar itself, not by `Match`:
# `X::Foo::G.parse($s).^name` is `X::Foo::G` in rakudo. mutsu has no complete
# exception hierarchy to consult, so ~20 sites decide "is this an exception?"
# from the class name alone (`Exception`, the `X::`/`CX::` namespaces). Every
# one of them mistook such a Match for an exception: it stringified to
# "X::Foo::G with no message", `+$/` numified THAT text into a Failure, and
# `$/ ~~ Exception` was True.
#
# Not a contrived namespace: `Crane` (the dependency `Config::TOML` is built
# on) declares a grammar inside `class X::Crane::PathOutOfRange` to parse the
# `Range` out of an `X::OutOfRange`, so its `method integer($/) { make(+$/) }`
# action made a Failure and every out-of-range path error died rendering its
# own message.
#
# Verified against rakudo v2026.07: the `X::`-namespaced grammar's answers
# below are identical to the plain one's, which is the whole point.

plan 26;

grammar Plain::G {
    token integer { '-'? \d+ }
    token TOP { ^ <integer> $ }
}

grammar X::Deep::G {
    token integer { '-'? \d+ }
    token TOP { ^ <integer> $ }
}

grammar CX::Deep::G {
    token TOP { ^ \d+ $ }
}

for (Plain::G, 'Plain::G'), (X::Deep::G, 'X::Deep::G') -> ($grammar, $label) {
    my $m = $grammar.parse('42');
    ok($m.defined, "$label parses");
    is($m.Str, '42', "$label: .Str is the matched text");
    is(~$m, '42', "$label: ~ is the matched text");
    is($m.Numeric, 42, "$label: .Numeric numifies the matched text");
    is(+$m, 42, "$label: prefix + numifies the matched text");
    is($m.Int, 42, "$label: .Int numifies the matched text");
    is($m.chars, 2, "$label: .chars counts the matched text");
    ok($m ~~ Match, "$label: the result is a Match");
    nok($m ~~ Exception, "$label: the result is NOT an Exception");
    is($m<integer>.Str, '42', "$label: its capture reads back");
}

# `CX::` is gated by the same namespace rule.
{
    my $m = CX::Deep::G.parse('7');
    is(+$m, 7, 'CX::-namespaced grammar: prefix + numifies the matched text');
    nok($m ~~ Exception, 'CX::-namespaced grammar: the result is NOT an Exception');
}

# An actual exception in those namespaces still reads as one.
{
    my $e = X::AdHoc.new(:payload<boom>);
    ok($e ~~ Exception, 'a real X:: exception still smartmatches Exception');
    is($e.message, 'boom', 'and still renders its message');
}

# The action-method shape the whole thing was found through: `make(+$/)` in a
# grammar whose own package sits under `X::`.
{
    class X::Deep::Acts {
        method integer($/ --> Nil) { make(+$/) }
        method TOP($/ --> Nil) { make($<integer>.made) }
    }
    is(
        X::Deep::G.parse('42', :actions(X::Deep::Acts.new)).made,
        42,
        'an action method of an X::-namespaced grammar can make(+$/)',
    );
    isa-ok(
        X::Deep::G.parse('42', :actions(X::Deep::Acts.new)).made,
        Int,
        'and what it made is an Int, not a Failure',
    );
}

# done
