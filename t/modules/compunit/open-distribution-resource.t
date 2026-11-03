use v6;
use lib 't/lib/ResSplit/lib';
use Test;

plan 3;

# `open(%?RESOURCES<x>)` (routine form) opens the resource's file instead of a
# file named after the stringified Distribution::Resource object (#16155).
use ResSplit;

my $r = strs();
my $expected = $r.slurp;
my $fh = open($r);
isa-ok $fh, IO::Handle, 'open() of a resource returns a handle';
is $fh.slurp, $expected, '...reading the resource file';
$fh.close;
is open($r, :bin).slurp(:bin).elems, $expected.encode.elems, 'open takes named arguments';
