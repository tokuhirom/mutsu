# Fixture for t/modules/import-export/use-in-block-export-hook-runs-once.t: a
# `sub EXPORT` that says each time it runs.
sub EXPORT(|) {
    say "EXPORT ran";
    Map.new('&hook-value' => sub { 42 })
}
