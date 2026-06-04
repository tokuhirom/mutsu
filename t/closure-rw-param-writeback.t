use Test;

# Regression: a closure with a writable parameter (sigilless `\a`, `is rw`, or
# `is raw`) writes a mutation back into the caller through the *source
# container's* env name. That name is neither a free variable nor a parameter,
# so the closure-exit write-back skip (added for read-only-closure perf) must
# not skip the env scan for such closures -- otherwise a mutation is silently
# dropped when a prior, non-mutating writable-param call has left `env_dirty`
# clear. Triggered here via the hyper function-op, which binds the left lvalue
# by reference through a synthetic source variable.

plan 6;

sub keep (\a, \b) { "kept" }     # writable param, does NOT mutate
sub bump (\a, \b) { a = "bumped" }  # writable param, mutates

# A non-mutating writable-param call first, then a mutating one: the second
# call's write-back must still reach the lvalue.
{
    my &k = &keep;
    my &m = &bump;
    my $a = 1;
    is ($a >>[&k]<< 0), "kept", "non-mutating writable-param call returns result";
    is $a, 1, "non-mutating call leaves lvalue unchanged";
    my $b = 1;
    is ($b >>[&m]<< 0), "bumped", "mutating writable-param call returns result";
    is $b, "bumped", "mutating call writes back even after a prior non-mutating one";
}

# Two mutating calls in a row also both write back.
{
    my &m = &bump;
    my $x = 1;
    $x >>[&m]<< 0;
    my $y = 2;
    $y >>[&m]<< 0;
    is $x, "bumped", "first lvalue written back";
    is $y, "bumped", "second lvalue written back";
}
