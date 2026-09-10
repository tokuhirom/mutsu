use Test;

# A user-declared `multi sub trait_mod:<is>(Routine:D $r, :$name!)` was never
# actually invoked for `is name` on a `sub` declaration when `name` happened
# to be one mutsu's parser also recognises natively (e.g. `test-assertion`):
# the parser consumed the trait itself and never queued it as a custom trait,
# so no dispatch to the user handler ever happened. See
# news/2026-08/test-assertion-trait-is-not-introspectable.md item 1. This is the
# `sub`-declaration analogue of `t/user-trait-mod-does-not-consume-every-trait.t`
# (which covers the variable-trait side of the same ordering rule).

plan 4;

use MONKEY-SEE-NO-EVAL;

# The user handler actually runs, with visible side effects, for a trait name
# mutsu's parser also treats as a builtin (`test-assertion`).
my $seen = False;
multi sub trait_mod:<is>(Routine:D $r, :$test-assertion!) { $seen = True }
sub foo-ok() is test-assertion { }
ok $seen, 'user trait_mod:<is> handler runs for a natively-recognised trait name';

# A `die` from INSIDE a matching handler is a real error and must propagate,
# not be silently swallowed the way a "no candidate matched" verdict is.
multi sub trait_mod:<is>(Routine:D $r, :$explodes!) { die "boom from sub handler" }
throws-like 'sub bad() is explodes { }', X::AdHoc,
    'an error from inside a matching sub trait_mod:<is> handler still propagates';

# `is test-assertion` with no user handler in scope at all keeps working
# (mutsu's own builtin meaning), both outside EVAL...
lives-ok { EVAL 'sub also-ok() is test-assertion { }' },
    'is test-assertion still applies with no user trait_mod:<is> in scope';

# ...and a handler that mixes a role onto the routine (`$r does Role`) is
# still visible via `&name` afterwards, in addition to what it triggers.
role is-marked { method is-marked(--> True) { } }
multi sub trait_mod:<is>(Routine:D $r, :$marked!) { $r does is-marked }
sub bar() is marked { 1 }
ok &bar.can("is-marked").Bool, 'the does-writeback from a sub trait handler still applies';

done-testing;
