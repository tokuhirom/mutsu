use Test;

plan 8;

# Rakudo keeps the numeric type of the temporal invocant: a zero is an Int,
# any other element a Num, and a Duration's first remainder stays a Duration.
is Instant.from-posix(100).polymod(60, 60).raku, '(50e0, 1e0, 0).Seq', 'Instant.polymod';
is Instant.from-posix(100).polymod(60).raku, '(50e0, 1e0).Seq', 'Instant.polymod one divisor';
is Instant.from-posix(-10).polymod(60, 60).raku, '(0, 0, 0).Seq', 'Instant.polymod zero';
is Duration.new(3725).polymod(60, 60).raku, '(Duration.new(5.0), 2e0, 1e0).Seq', 'Duration.polymod';
is Duration.new(3725.5).polymod(60, 60).raku, '(Duration.new(5.5), 2e0, 1e0).Seq', 'Duration.polymod fraction';
is Duration.new(30).polymod(60, 60).raku, '(Duration.new(30.0), 0, 0).Seq', 'Duration.polymod small';
is Duration.new(60).polymod(60, 60).raku, '(0, 1e0, 0).Seq', 'Duration.polymod zero remainder';
is Duration.new(3725).polymod(60).raku, '(Duration.new(5.0), 62e0).Seq', 'Duration.polymod one divisor';
