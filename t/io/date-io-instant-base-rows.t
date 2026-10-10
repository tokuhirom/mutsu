use Test;

# Date.IO, DateTime.IO and the base of Instant and Duration are rows of the
# method table (ADR-11276 section 9.58).

plan 8;

my $d = Date.new(2024, 1, 2);
isa-ok $d.IO, IO::Path, 'Date.IO is an IO::Path';
is $d.IO.basename, '2024-01-02', '... named after the date';
is $d.IO(:CWD('/tmp')).basename, '2024-01-02', ':CWD is accepted and ignored';
is DateTime.new(2024, 1, 2, 3, 4, 5).IO.basename, '2024-01-02T03:04:05Z', 'DateTime.IO';

is Instant.from-posix(1).base(10), '11', 'Instant.base(10)';
is Duration.new(1.5).base(2), '1.1', 'Duration.base(2)';
is Duration.new(1.5).base(10, 3), '1.500', 'Duration.base with fractional digits';
is Duration.new(1.5).base(10, *), '1.5', 'Duration.base with *';
