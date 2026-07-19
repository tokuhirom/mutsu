use Test;

# Pin for the (B) per-store env-write gate mechanism #3 fix
# (docs/lexical-scope-slot-campaign.md, "Root-cause drill-down").
#
# A scalar local holding an aggregate must survive a Test-fn call that follows
# it. Under the MUTSU_GATE_LOCAL_ENV_WRITE gate the plain-lexical store skips its
# env mirror, so env keeps the `my $x` decl seed (Any). The Test fn dispatches
# through the carrier fallback, whose `carrier_writeback_changed_aggregates`
# "type change away from container" branch would read that stale Any and clobber
# the live aggregate slot. The fix requires env to have genuinely changed during
# the carrier before that write. This test passes gate-OFF (the default) and
# would fail gate-ON before the fix.

plan 6;

my $l = List.new(1, 2, 3);
isa-ok $l, List, 'List local isa List after a Test-fn call';
is $l.elems, 3, 'List local keeps its elements across the Test-fn call';

my @a = 1, 2, 3, 4;
ok @a.elems == 4, 'Array local survives a preceding Test-fn assertion';
is @a.elems, 4, 'Array local still has 4 elems after another Test-fn call';

my %h = a => 1, b => 2;
isa-ok %h, Hash, 'Hash local isa Hash after a Test-fn call';
is %h.elems, 2, 'Hash local keeps its pairs across the Test-fn call';
