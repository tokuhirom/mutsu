# Fixture for t/modules/import-export/use-hook-runs-once-per-use.t: a module
# whose `sub EXPORT` says each time it runs.
say "HookRunsOncePerUse loaded";
sub EXPORT(*@args) {
    say "EXPORT ran with ({@args.join(',')})";
    Map.new: '&hook-runs-once' => sub { "imported" }
}
