use v6;
use Test;
use lib 't/lib';
use ExportQualifiedKeys;

# #16555: the bare `::` pseudo-stash lists what the importing scope's pad
# holds, like `MY::` does, including a qualified key a `sub EXPORT` hash
# installed. `::{"Pkg::Name"}` read nothing before, so a module that looks its
# exported values up with a computed key (Universal::Errno) found `Nil`.

plan 7;

my $k = 'A';
is ::{"X::ExportQualifiedKeys::Th::$k"}.v, 1, 'a computed key finds the exported value';
is ::{'X::ExportQualifiedKeys::Th::B'}.v, 2, 'a literal key too';
ok ::{'X::ExportQualifiedKeys::Th::A'}:exists, ':exists sees the key';
nok ::{'X::ExportQualifiedKeys::Th::Nope'}:exists, 'and not an absent one';
ok ::.keys.grep('X::ExportQualifiedKeys::Th::A'), 'the stash lists the key';
is-deeply MY::.keys.grep(*.starts-with('X::ExportQualifiedKeys::Th::')).sort.List,
    ('X::ExportQualifiedKeys::Th::A', 'X::ExportQualifiedKeys::Th::B'),
    'MY:: lists the same names';

# Ordinary lexicals are still there.
my $plain = 42;
is ::<$plain>, 42, 'a plain lexical is still read through ::';
