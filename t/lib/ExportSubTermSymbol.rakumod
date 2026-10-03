my $calls = 0;
sub answer(--> Int:D) { ++$calls; 42 }
sub calls() { $calls }

# The Today dist's shape: a term exported through `sub EXPORT`.
sub EXPORT() {
    Map.new('&term:<answer>' => &answer, '&term:<answer-calls>' => &calls)
}
