sub EXPORT(|) {
    my $candidate := multi routine-export-local-multi(Int $x) { "int:$x" };
    Map.new
}
