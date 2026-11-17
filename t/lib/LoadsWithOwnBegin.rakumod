# Fixture for t/modules/import-export/use-loads-at-parse-time.t: a module
# whose own compile-time effects (a `BEGIN`, a `multi`) belong to it alone,
# not to the unit that `use`s it.
BEGIN my $seen = 1;
multi sub own-begin-multi() is export { "multi" }
say "LoadsWithOwnBegin loaded";
