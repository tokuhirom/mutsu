# Fixture for t/modules/import-export/use-loads-at-parse-time.t: the module
# says when it loads, so the test sees when (and how often) that happens.
say "LoadsAtParseTime loaded";
sub loads-at-parse-time is export { "imported" }
