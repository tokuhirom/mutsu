# A `Test::`-namespaced module with a `sub EXPORT`, like the ecosystem's
# Test::When (`use Test::When <smoke>`).
sub EXPORT(*@args) {
    Map.new('&test-ns-export-args' => sub { @args.join(',') })
}
