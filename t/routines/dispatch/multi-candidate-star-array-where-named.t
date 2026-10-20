use Test;

# From the Red distribution (`red-do`): a `where` on a positional `*@b` slurpy
# must not see the named arguments of the call.
plan 4;

multi rd(*@b where .all ~~ Callable, Str :$with = "default", :$async) { "m1 with=$with" }
multi rd(&b, Int:D :$with, :$async where not *) { "m2" }
is rd({;}), "m1 with=default", 'no named argument';
is rd({;}, :with<x>), "m1 with=x", 'named argument is not part of @b';

multi rf(*@b where .all ~~ Int, :$k) { "ints $k" }
is rf(1, 2, :k<v>), "ints v", 'where over the positionals only';

proto rp(|) { {*} }
multi rp(*@b where .all ~~ Callable, Str :$with = "d") { "p $with" }
is rp({;}, :with<y>), "p y", 'through a proto';
