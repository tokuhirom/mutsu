unit module JSON::Fast;

# A stand-in JSON::Fast placed on the module-resolution ladder, used by
# t/modules/batteries/json-module-ladder.t to prove that mutsu's native
# JSON::Fast provider is a LAST-RESORT fallback and not an override: when a
# `JSON::Fast` really is reachable via `use lib` / `-I` / `MUTSULIB` / the site
# repo, it is the one that runs (BATTERIES.md §6).
#
# Its answers are deliberately unlike the native provider's so the difference
# is observable from the test.

sub to-json($d) is export { "ladder-to-json:" ~ $d.join(",") }
sub from-json(Str $text) is export { "ladder-from-json:$text" }
