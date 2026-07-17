use Test;

# Raku's `Nil` has a `FALLBACK` that returns `Nil` for any method it does not
# define, so `Nil.ast` is `Nil`. mutsu implemented that on the scalar method-call
# path only: the hyper path (`».ast`) went straight to dispatch and raised
# "No such method 'ast' for invocant of type 'Any'". That silently aborted
# grammar action methods doing `@<op>».ast` over a capture that never matched
# (99problems-41-to-50.t P47).

plan 10;

is (Nil,)>>.ast.raku,      '(Nil,)', 'hyper .ast over a Nil element yields Nil, not an error';
is Nil.ast.raku,           'Nil',    'scalar .ast on Nil still yields Nil';
is (Nil, Nil)>>.made.raku, '(Nil, Nil)', 'hyper .made over Nils';
is (Nil,)>>.no-such-method-anywhere.raku, '(Nil,)', 'arbitrary unknown method absorbs to Nil';

# The FALLBACK must not swallow methods Nil genuinely defines.
is (Nil,)>>.defined.raku,  '(Bool::False,)', '.defined is answered, not absorbed';
is (Nil,)>>.Str.raku,      '("",)',          '.Str is answered, not absorbed';

# Non-Nil elements are unaffected.
is (1, 2)>>.succ.raku,     '(2, 3)',         'ordinary hyper dispatch unaffected';

# A real missing method on a non-Nil invocant must still throw.
dies-ok { (Any,)>>.no-such-method-anywhere }, 'Any (not Nil) still throws for an unknown method';
dies-ok { Any.no-such-method-anywhere },      'scalar Any still throws for an unknown method';

# Nested Iterables recurse to the leaves, and Nil leaves absorb there too. Each
# descent itemizes its result, so the sub-lists come back as `$(Nil,)` — see
# t/hyper-nested-itemize.t.
is ((Nil,), (Nil,))>>.ast.raku, '($(Nil,), $(Nil,))', 'nested hyper reaches Nil leaves';

# vim: expandtab shiftwidth=4
