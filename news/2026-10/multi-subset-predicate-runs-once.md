# A multi candidate's subset and `where` predicates run once per call

Calling a `multi` whose winning candidate has a user-subset-typed parameter ran
the subset's predicate twice per call: once when dispatch matched the arguments
to pick the candidate, and again when the binder bound the winner. Multi
*methods* did the same for `where` clauses too. With a side-effecting predicate
(`subset P of Int where { $c++; True }`) the counter advanced by two per call
where Rakudo advances it by one, and every such call paid for the predicate
twice.

The trust that #8697 introduced for a multi sub's `where` clauses
(`pending_skip_constraint_recheck`, renamed from `pending_skip_where_recheck`)
now covers subset-typed parameters, positional and named, and is armed for a
multi method's winner as well: both the general method binder and its fast
path accept the verdict dispatch already reached instead of re-running the
user's code (#10986).
