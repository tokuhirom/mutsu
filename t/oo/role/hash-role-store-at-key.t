use v6;
use Test;

plan 14;

# From the Hash-with distribution: a role mixed onto a Hash overrides the
# Associative protocol, and the initializer of `my %h does R = ...` is the
# mixed container's own STORE, which stores every pair through STORE_AT_KEY.

role Lc {
    method AT-KEY(::?ROLE:D: \key)              { nextwith(key.lc)        }
    method EXISTS-KEY(::?ROLE:D: \key)          { nextwith(key.lc)        }
    method DELETE-KEY(::?ROLE:D: \key)          { nextwith(key.lc)        }
    method STORE_AT_KEY(::?ROLE:D: \key,\value) { nextwith(key.lc, value) }
    method BIND-KEY(::?ROLE:D: \key,\value)     { nextwith(key.lc, value) }
}

my %h does Lc = "A", 42;
is %h<a>, 42, 'initializer keys go through STORE_AT_KEY';
is %h<A>, 42, 'lookup goes through AT-KEY';
is-deeply %h<A>:exists, True, 'EXISTS-KEY override';
is %h.keys, "a", 'the stored key was mapped';
is-deeply (%h<A> := 666), 666, 'BIND-KEY override';
is-deeply %h<A>:delete, 666, 'DELETE-KEY returns the removed value';
is %h.elems, 0, 'now empty';

my %g does Lc;
%g = B => 1;
is %g.keys, "b", 'list assignment stores through STORE_AT_KEY';

my %i does Lc = (D => 4);
is %i.keys, "d", 'parenthesised pair initializer';

role HW[&mapper] {
    method AT-KEY(::?ROLE:D: \key)              { nextwith(mapper(key))        }
    method EXISTS-KEY(::?ROLE:D: \key)          { nextwith(mapper(key))        }
    method STORE_AT_KEY(::?ROLE:D: \key,\value) { nextwith(mapper(key), value) }
}
sub ordered($a) { $a.comb.sort.join }
my %o does HW[&ordered] = oof => 42;
is %o<foo>, 42, 'parametric role: lookup by sorted key';
is %o<ofo>, 42, 'parametric role: lookup by permuted key';
is %o.keys, "foo", 'parametric role: stored key';
is-deeply %o<ofo>:exists, True, 'parametric role: exists';

my @a does Positional = 1, 2, 3;
is @a.elems, 3, 'array `does` with initializer still works';
