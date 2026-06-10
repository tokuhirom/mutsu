use Test;

# The `:` invocant marker may only follow the FIRST parameter. On any later
# parameter (`sub f($x, $y:)`, `method m($x, $y:)`) Raku raises
# X::Syntax::Signature::InvocantMarker, regardless of sub vs method.

plan 5;

throws-like 'my sub f($x, $y:) { }', X::Syntax::Signature::InvocantMarker,
    'invocant marker on a non-first sub parameter';

throws-like 'class C { method m($x, $y:) { } }', X::Syntax::Signature::InvocantMarker,
    'invocant marker on a non-first method parameter';

# A method with the invocant marker on the first parameter is legal.
lives-ok {
    class C { method m($self: $x) { $x * 2 } }
    die unless C.new.m(5) == 10;
}, 'invocant marker on the first method parameter works';

# Ordinary subs/methods without markers are unaffected.
lives-ok { sub g($x, $y) { $x + $y }; die unless g(2, 3) == 5; },
    'plain two-positional sub works';

# A sub may not have an invocant at all (distinct error, still rejected).
throws-like 'my sub h($self:) { }', X::Syntax::Signature::InvocantNotAllowed,
    'sub with a first-parameter invocant marker is InvocantNotAllowed';
