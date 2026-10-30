use Test;

# From Path::Finder: `Path::Finder.skip-vcs` calls `self.skip-dir($junction)` on
# the type object. A `Mu` parameter takes a junction whole, whether the
# invocant is an instance or a type object.
plan 7;

class F {
    method mu(Mu $p) { 'mu' }
    method any-typed(Any $p) { 'any' }
    method untyped($p) { 'untyped' }
    method with-default(Mu $p, Bool $hide = True) { 'default' }
}

is F.mu(any(1, 2)), 'mu', 'type object, Mu parameter';
is F.new.mu(any(1, 2)), 'mu', 'instance, Mu parameter';
is F.with-default(any(1, 2)), 'default', 'type object, Mu parameter with a later default';
is-deeply F.any-typed(any(1, 2)), any('any', 'any'), 'type object, Any parameter threads';
is-deeply F.untyped(any(1, 2)), any('untyped', 'untyped'), 'type object, untyped parameter threads';
is-deeply F.new.any-typed(any(1, 2)), any('any', 'any'), 'instance, Any parameter threads';
is-deeply F.new.untyped(any(1, 2)), any('untyped', 'untyped'), 'instance, untyped parameter threads';

# vim: expandtab shiftwidth=4
