use Test;

# A coercion type `T(F)` introduces *two* candidates (S06-multi/type-based.t):
# one taking the target `T` directly, and one taking the accepted type `F`
# (`Any` when omitted) and coercing.  So a coercion parameter is as narrow as
# its target only when the argument already IS a target, and is otherwise as
# wide open as `F`.

plan 15;

{
    proto sub f(|) {*}
    multi sub f(Str() $s)  { 'str' }
    multi sub f(Blob:D $b) { 'blob' }

    is f('abc'.encode), 'blob', 'Blob:D beats Str() for a Blob argument';
    is f('abc'), 'str', 'Str() wins for a Str argument';
    is f(42), 'str', 'Str() takes an Int nobody else claims';
}

{
    # The OpenSSL::Digest shape: the Str() candidate delegates to the Blob:D
    # one, so picking Str() for a Blob argument was an infinite mutual
    # recursion, not merely a wrong answer.
    proto sub digest(|) {*}
    multi sub digest(Str() $s)  { digest($s.encode) }
    multi sub digest(Blob:D $b) { $b.elems }

    is digest('abcd'), 4, 'a Str delegates once to the Blob candidate';
    is digest('abcd'.encode), 4, 'a Blob goes straight to the Blob candidate';
}

{
    proto sub g(|) {*}
    multi sub g(Int() $n) { 'coerce' }
    multi sub g(Cool $c)  { 'cool' }

    is g(42), 'coerce', 'Int() is as narrow as Int for an Int argument';
    is g('42'), 'cool', 'Int() is only as narrow as Any for a Str argument';
}

{
    proto sub h(|) {*}
    multi sub h(Int(Cool) $n) { 'coerce' }
    multi sub h(Str $s)       { 'str' }

    is h('3'), 'str', 'Int(Cool) ranks by its accepted type Cool, losing to Str';
    is h(3), 'coerce', 'Int(Cool) ranks by its target Int for an Int argument';
}

{
    proto sub k(|) {*}
    multi sub k(Int() $n) { 'int' }
    multi sub k(Str() $s) { 'str' }

    is k(42), 'int', 'the coercion whose target matches wins';
    is k('42'), 'str', 'the coercion whose target matches wins (Str)';
    throws-like { k(4e0) }, X::Multi::Ambiguous,
        'two coercions that both only accept Any are ambiguous';
}

{
    # An unconstrained positional IS an `Any` positional.
    proto sub m(|) {*}
    multi sub m(Any $x) { 'any' }
    multi sub m($y)     { 'bare' }

    throws-like { m(42) }, X::Multi::Ambiguous, 'Any $x and bare $y are ambiguous';
}

{
    proto sub n(|) {*}
    multi sub n(Str() $x) { 'coerce' }
    multi sub n($y)       { 'bare' }

    throws-like { n(42) }, X::Multi::Ambiguous,
        'Str() and a bare param both accept Any for a non-Str argument';
    is n('x'), 'coerce', 'Str() out-narrows a bare param for a Str argument';
}
