use Test;

plan 4;

is Duration.new(0).Bool, False, 'a zero Duration is false';
is Duration.new(0.5).Bool, True, 'a non-zero Duration is true';
is so(Duration.new(-3)), True, 'a negative Duration is true';
is Instant.from-posix(0).Bool, True, 'an Instant is true even at the epoch';
