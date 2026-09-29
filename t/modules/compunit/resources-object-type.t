use v6;
use lib 't/lib/ResInner/lib';
use Test;

plan 5;

# Distribution::Resources::Menu (ecosystem distribution) declares
# `has Distribution::Resources $.resources is required` and stores
# `%?RESOURCES` in it; mutsu handed over a plain Hash and the type check failed.
use ResInnerProbe;

my $r = resources();
is $r.^name, 'Distribution::Resources', '%?RESOURCES is a Distribution::Resources';
ok $r ~~ Distribution::Resources, 'smartmatches its own type';

class Holder { has Distribution::Resources $.res is required }
lives-ok { Holder.new(res => resources()) }, 'accepted by a typed attribute';
is $r<greeting.txt>.slurp.trim, 'hello from the ResInner resources',
    'still subscriptable as a map of resource paths';

# Distribution::Resources::Menu builds nested hashes through the routine form
# of the multi-dimensional subscript.
my %h;
postcircumfix:<{; }>(%h, <a b c>) = 'x';
postcircumfix:<{ }>(%h, 'k') = 'y';
is-deeply %h, { a => { b => { c => 'x' } }, k => 'y' },
    'postcircumfix routines are assignable';
