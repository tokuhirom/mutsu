use Test;

# A method called on a role GROUP's type object -- a parametric candidate plus
# a plain one, as CSS::Units declares -- runs on the plain candidate's pun.
# The pun used to be composed under the role's own name and withdrawn again
# around every call. With the registry shared between threads, one thread's
# withdrawal removed the class another thread was dispatching through, so the
# call died with "No such method 'value'" (#18160, CSS::Properties t/threads.t).

plan 5;

role U[\d, \u] { method units { u } }
role U { method value(\v, \u) { v but U[1, u] } }

is U.value(3, 'pt').units, 'pt', 'a role group type object method runs on the plain candidate';

my $failures = 0;
for ^30 {
    (^8).race(:batch(1)).map({ U.value($_, 'pt'); my $p = U[1, 'pt']; 1 }).eager;
    CATCH { default { $failures++ } }
}
is $failures, 0, 'concurrent group method calls and parameterizations do not race';

my @units = (^8).race(:batch(1)).map({ U.value($_, 'px').units });
is-deeply @units.sort.unique.List, ('px',), 'every worker sees the same pun';

# The candidate order does not matter: the plain candidate is still selected.
role W { method twice($x) { $x * 2 } }
role W[::T] { method t { T } }
is W.twice(21), 42, 'plain candidate declared first';

# The group's type object is still a role, not a class.
nok W.HOW.^name.contains('ClassHOW'), 'the group stays a role after a method call';
