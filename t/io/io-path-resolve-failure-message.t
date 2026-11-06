use Test;
plan 4;

my $p = "no-such-dir-xyz/../bar".IO;
my $f = $p.resolve(:completely);
isa-ok $f, Failure, 'resolve(:completely) of a missing directory fails';
is $f.exception.^name, 'X::IO::Resolve', 'with X::IO::Resolve';
is $f.exception.path.^name, 'IO::Path', 'whose path is the IO::Path itself';
is $f.exception.message, "Failed to completely resolve {$p.raku}",
    'and the message renders it with .raku';
$f.so;
