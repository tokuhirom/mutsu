use Test;

# `.+` / `.*` on a *type object* of a user-defined class must walk the whole MRO
# and call every level's method, exactly like on an instance. Previously a type
# object fell through to the single-dispatch path and only the most-derived
# method ran (`C.+f` yielded `(3)` instead of `(3 2 1)`).

plan 10;

class A { method f { 1 } }
class B is A { method f { 2 } }
class C is B { method f { 3 } }

# .+ walks the full MRO on a type object.
is-deeply C.+f, (3, 2, 1), '.+ on a type object walks the MRO';
is-deeply B.+f, (2, 1), '.+ on a two-level type object';
is-deeply A.+f, (1,), '.+ on a single-level type object';

# .* behaves the same for a method that exists.
is-deeply C.*f, (3, 2, 1), '.* on a type object walks the MRO';

# .* on a missing method yields the empty list (no error).
is-deeply A.*nonesuch, (), '.* on a missing method is ()';

# .+ on a missing method dies.
dies-ok { A.+nonesuch }, '.+ on a missing method dies';

# The instance path is unchanged.
is-deeply C.new.+f, (3, 2, 1), '.+ on an instance still walks the MRO';
is-deeply B.new.*f, (2, 1), '.* on an instance still walks the MRO';

# Only the levels that define the method contribute.
class P { method g { 'p' } }
class Q is P { }              # Q does not define g
class R is Q { method g { 'r' } }
is-deeply R.+g, ('r', 'p'), '.+ skips levels that do not define the method';

# The result is a List.
ok C.+f ~~ List, '.+ returns a List';
