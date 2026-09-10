use Test;

# (B) per-store env-write gate (MUTSU_GATE_LOCAL_ENV_WRITE) regression pin.
#
# A frame that installs a resume-safe CONTROL handler
# (`CONTROL { default { ...; .resume } }`) has its handler run INLINE at a deep
# `warn` raise site, which reconstructs the installing frame's locals FROM ENV by
# name (self.locals is the deep raise-site frame). Under the gate a plain
# `my $out = ''` in that frame skips its env mirror, so the handler reconstructs a
# stale `$out` and its mutation is lost. compute_needs_env_sync now folds every
# local of such a frame back into needs_env_sync (gated; OFF byte-identical).
# This pin passes OFF, ON (MUTSU_GATE_LOCAL_ENV_WRITE=1), and under real raku.

plan 5;

{
    my $out = '';
    CONTROL { default { $out ~= "[{.message}]"; .resume; } }
    warn "direct";
    is $out, '[direct]', 'direct warn mutates the captured $out';
    pass 'execution continues after a direct warn';
}

{
    my $out = '';
    CONTROL { default { $out ~= "[{.message}]"; .resume; } }
    my $w = &warn;
    $w.("indirect");
    is $out, '[indirect]', 'indirect warn via &warn.() mutates $out';
}

{
    my @seen;
    CONTROL { default { @seen.push(.message); .resume; } }
    my $w = &warn;
    $w.("one");
    $w.("two");
    is @seen.join(','), 'one,two', 'consecutive indirect warns accumulate';
}

# A running counter mutated across several resumed warns.
{
    my $n = 0;
    CONTROL { default { $n++; .resume; } }
    warn "a";
    warn "b";
    warn "c";
    is $n, 3, 'counter accumulates across three resumed warns';
}
