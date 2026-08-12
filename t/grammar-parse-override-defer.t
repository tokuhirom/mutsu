use Test;

# ADR-0019 E9-pre ground-truth pin (verified against Rakudo v2026.06, 2026-08-12):
# a Grammar .parse override reaches the real parse via callsame/nextsame and the
# Match flows back through the deferral.

plan 4;

grammar G {
    token TOP { (\d+) }
    method parse(|c) { my $m = callsame; $m }
}
my $m = G.parse("123");
ok ?$m, "callsame in a parse override reaches the real parse";
is ~$m, "123", "match text flows back through callsame";

grammar G2 {
    token TOP { \w+ }
    method parse(|c) { nextsame }
}
my $m2 = G2.parse("abc");
ok ?$m2, "nextsame in a parse override reaches the real parse";
is ~$m2, "abc", "match text flows back through nextsame";
