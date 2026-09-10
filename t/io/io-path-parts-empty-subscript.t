use Test;

plan 1;

my $parts = IO::Path::Parts.new('C:', '/some/dir', 'foo.txt');
is $parts[].map(*.gist).join(', '), 'volume => C:, dirname => /some/dir, basename => foo.txt',
    'an empty positional subscript returns all IO::Path::Parts elements';
