use lib 't/lib';
use Test;

plan 3;

# NativeCall exports `bool`, but a `use` is lexical: the module that used it
# does not hand it to its own importer (Pakku::Fetch -> Pakku::Core).
use NativeCallImportHolder;

my sub bool($name, $value) { "$name=$value" }

my %table =
    A => [ 'a', { bool 'A', $_ } ],
    B => [ 'b', { bool 'B',   $_ } ];

is %table<A>[1](5), 'A=5', 'a user sub named like a NativeCall type parses after use of a user of NativeCall';
is %table<B>[1](6), 'B=6', 'a second entry in the same hash literal parses too';
is NativeCallImportHolder.new.size, 4, 'the module itself still sees NativeCall';
