use Test;

# (B) per-store env-write gate (MUTSU_GATE_LOCAL_ENV_WRITE) regression pin.
#
# A regex literal that interpolates a lexical (`/ ... $script ... /`) may be
# matched in a DIFFERENT frame — e.g. `like $err, / ... $script ... /` matches
# inside `like`, whose interpolate_regex_scalars resolves `$script` from the
# name-keyed env (the cross-frame store), not the constructing frame's slots.
# Under the gate a plain `my $script = ...` skips its env mirror, so the
# interpolation read a stale/empty value and the match failed. compute_needs_env_sync
# now folds every local of a frame holding an interpolating regex constant back
# into needs_env_sync (gated; OFF byte-identical). Passes OFF, ON, and real raku.

plan 5;

# The failing shape from test-assertion-line-number.t: a lexical interpolated
# into a regex matched by the `like` builtin (a different frame).
{
    my $script = 'myfile.raku';
    my $err := "at myfile.raku line 4\n";
    like $err, /'at ' $script ' line ' 4/, 'lexical interpolated in a like regex';
}

# Direct cross-frame match via a user sub.
{
    my $needle = 'wanted';
    sub finds($hay, $rx) { $hay ~~ $rx }
    ok finds("has wanted here", /$needle/), 'lexical interpolated in a regex matched cross-frame';
}

# Inline match in the same frame still works.
{
    my $pat = 'abc';
    ok 'xabcx' ~~ /$pat/, 'inline interpolated regex match';
}

# The interpolated value tracks reassignment.
{
    my $p = 'one';
    $p = 'two';
    ok 'the two here' ~~ /$p/, 'reassigned lexical interpolates its live value';
    nok 'the one here' ~~ /$p/, 'old value no longer matches';
}
