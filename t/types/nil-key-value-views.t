use v6;
use Test;

# `Any`'s key/value views on an undefined invocant are the empty List
# (raku: `multi method keys(Any:U:) { () }` and friends), so `Nil.keys` is
# `()` like `Any.keys` -- not a Nil absorbed by `Nil.FALLBACK`, and not a
# one-element list wrapping Nil. Issue #9127.

plan 20;

for <keys values kv pairs antipairs invert> -> $m {
    is-deeply Nil."$m"(), (), "Nil.$m is the empty List";
    is-deeply Any."$m"(), (), "Any.$m is the empty List";
}

is Nil.keys.sort.gist, '()', 'Nil.keys.sort is empty';
is Nil.keys.elems, 0, 'Nil.keys has no elements';
is Nil.pairs.WHAT.raku, 'List', 'Nil.pairs is a List';

# A Nil bound to a named container dispatches the same way.
{
    my $x := Nil;
    is-deeply $x.keys, (), 'bound Nil: .keys is the empty List';
    is-deeply $x.values, (), 'bound Nil: .values is the empty List';
}

# The hyper leaf agrees with the scalar call.
is-deeply (Nil,)».keys, ((),), '(Nil,)».keys';

# A failed parse (Nil) keyed, the case the issue was found with.
grammar H { token t { a } }
is H.parse('x', :rule<t>).keys.sort.gist, '()', 'failed parse .keys.sort is empty';

# Methods Nil does not define are still absorbed.
ok Nil.no-such-method === Nil, 'unknown methods on Nil still return Nil';
