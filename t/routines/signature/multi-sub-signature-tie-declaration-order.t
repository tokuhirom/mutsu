use Test;

# #11027: a destructuring sub-signature makes a multi candidate a bind-check
# candidate. Tied with another, the first declared that binds wins -- the
# `where` nested inside it is consulted, and the call is never ambiguous.
# Methods tie-break bind-check candidates (`where`, sub-signature) the same way.

plan 12;

multi sub k(@ ($x where * > 5)) { 'big' }
multi sub k(@ ($x)) { 'small' }
is k([9]), 'big', 'nested where admits the first candidate';
is k([1]), 'small', 'nested where rejects it, the next one binds';

multi sub j(@ ($x)) { 'small' }
multi sub j(@ ($y where * > 5)) { 'big' }
is j([9]), 'small', 'tied sub-signature candidates: the first declared wins';

class C {
    multi method k(@ ($x where * > 5)) { 'big' }
    multi method k(@ ($x)) { 'small' }
}
is C.k([9]), 'big', 'method: nested where admits the first candidate';
is C.k([1]), 'small', 'method: nested where rejects it, the next one binds';
is C.new.k([9]), 'big', 'method on an instance';

class D {
    multi method k(@ ($x)) { 'small' }
    multi method k(@ ($y where * > 5)) { 'big' }
}
is D.k([9]), 'small', 'method: the first declared tied candidate wins';

class E {
    multi method m($x where True) { 1 }
    multi method m($x where True) { 2 }
}
is E.m(1), 1, 'method: tied where-constrained candidates resolve by declaration order';

multi sub n(:key($k)) { 'a' }
multi sub n(:key($j)) { 'b' }
is n(:key(1)), 'a', 'named rename parens tie by declaration order';

multi sub p($x) { 1 }
multi sub p($y) { 2 }
throws-like { p(1) }, X::Multi::Ambiguous, 'plain positional duplicates stay ambiguous';

class F {
    multi method m($x) { 1 }
    multi method m($y) { 2 }
}
throws-like { F.m(1) }, X::Multi::Ambiguous, 'plain positional method duplicates stay ambiguous';

multi sub q(@ ($a, $b)) { 'two' }
multi sub q(@ ($a)) { 'one' }
is q([1]), 'one', 'a sub-signature arity mismatch skips the candidate';
