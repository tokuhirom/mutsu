use Test;

# A sub nested in a routine reads that routine's lexicals, even when a
# same-named sub declared at program scope or in a block captured a
# same-named variable of its own (#13613). The captured-cell bucket of
# ADR-0024 belongs to the declaration that captured it, not to every
# routine of the name.

plan 4;

my @u = 'main';
sub acc { 'program:' ~ @u.join }
is acc(), 'program:main', 'the program-scope sub reads the program variable';
sub mk2 { my @u = 'mk2'; my $r = acc(); sub acc { 'nested:' ~ @u.join }; $r }
is mk2(), 'nested:mk2', 'a nested sub called before its declaration reads its routine';

{
    my $c = { my @v = 1, 2; sub bacc() { 'block:' ~ @v.join(',') }; bacc() };
    is $c(), 'block:1,2', 'a block sub reads the block variable';
    sub mk3 { my @v = 'mk3'; my $r = bacc(); sub bacc { 'nested:' ~ @v.join }; $r }
    is mk3(), 'nested:mk3', 'a nested sub does not read the block sub cells';
}
