use Test;

plan 1;

my $io = class :: is IO::Handle {
    method marker { 42 }
}.new;
is $io.marker, 42, 'anonymous class with qualified is-trait parses and runs';
