use v6;
use Test;

# Pin for #8355: `new`/`bless` are not a dispatch position at all in rakudo
# when using the DEFAULT constructor -- named arguments are collected
# straight into `*%attrinit`, so a Junction argument is a plain VALUE to
# store, never an autothreading trigger. mutsu's method-argument autothread
# check did not special-case the type-object receiver `new`/`bless` targets,
# so it threaded the constructor call once per eigenstate instead, which was
# both silently wrong (an untyped attribute ended up holding a NESTED
# junction) and quadratic-or-worse in the number of junction arguments.

plan 7;

# A typed attribute: mutsu died with a type check failure before the fix,
# since each threaded call passed one eigenstate (an Int), not the junction.
class D { has Junction $.a }
is D.new(a => any(1, 2)).a.raku, 'any(1, 2)',
    'a typed Junction attribute stores the junction whole via .new';

# Untyped attributes: mutsu silently stored a NESTED junction before the fix.
class E { has $.a; has $.b }
my $e = E.new(a => any(1, 2), b => any(3, 4));
is $e.a.raku, 'any(1, 2)', 'the first untyped attribute stores its junction whole';
is $e.b.raku, 'any(3, 4)', 'the second untyped attribute stores its own junction whole';

# .bless behaves the same as the default .new.
class F { has Junction $.a; has Junction $.b }
my $f = F.bless(a => any(1, 2), b => any(3, 4));
is $f.a.raku, 'any(1, 2)', '.bless stores the first junction attribute whole';
is $f.b.raku, 'any(3, 4)', '.bless stores the second junction attribute whole';

# A class that DEFINES ITS OWN `new`/`bless` is an ordinary method dispatch,
# so a junction argument still autothreads per the normal per-parameter
# rules (a typed positional parameter threads; #8355's fix must not disable
# autothreading for user-defined new/bless, only the default constructor).
class Q {
    has $.x;
    method new(Int $x) { self.bless(x => $x) }
}
is Q.new(any(1, 2)).x.raku, 'any(1, 2)',
    'a user-defined new with a typed positional parameter still autothreads';

# The performance angle (#8355): threading was O(eigenstates^junctions), so
# two junction arguments used to take visibly longer than a handful of
# milliseconds even at a small eigenstate count. This must return instantly.
class C { has Junction $.a; has Junction $.b }
my $start = now;
C.bless(a => any(^60), b => any(^60));
ok now - $start < 1, '.bless with two junction attributes returns quickly (not O(n^2) threading)';
