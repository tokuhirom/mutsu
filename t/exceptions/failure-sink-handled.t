use Test;
plan 4;

# .so marks a Failure as handled; sinking a handled Failure is a no-op
my $f := sub { fail "boom" }();
is $f.so, False, '.so on a Failure returns False';
lives-ok { $f.sink }, 'sinking a handled Failure does not throw';

# sinking an unhandled Failure throws its exception
dies-ok { sub { fail "kaboom" }().sink }, 'sinking an unhandled Failure throws';

# is-approx with both tolerances requires BOTH to pass
todo 'rel-tol fails even though abs-tol passes';
is-approx 1, 10, :abs-tol<90>, :rel-tol<.5>;
