use Test;
use nqp;

plan 7;

# An `@` parameter bound to an itemized Array that arrives without a variable
# holder (a method result, an element of a Map subclass built over shared
# `$!storage`) takes the Array's elements, not one element holding the Array
# (RepositoryEvent via JSON::RepositoryEvent, #16357).
my sub elem-types(@a) { @a.map({ .WHAT.^name }).join(',') }

my %h = a => [1, 2];
is elem-types(%h.AT-KEY('a')), 'Int,Int', 'AT-KEY result';
is elem-types(%h.values[0]), 'Int,Int', 'a .values element';
is elem-types(%h.pairs[0].value), 'Int,Int', 'a pair value';

class EP is Map { }
my %src = commits => [ { message => "deep" }, ];
my $e = nqp::p6bindattrinvres(nqp::create(EP), Map, '$!storage',
    nqp::getattr(nqp::decont(%src), Map, '$!storage'));
is elem-types($e<commits>), 'Hash', 'element of a Map subclass over bound storage';

my sub storage-type(@array) {
    @array.map({ nqp::getattr(nqp::decont($_), Map, '$!storage') }).head.WHAT.^name
}
is storage-type($e<commits>), 'Hash', "the element's storage is the hash, not an IterationBuffer";
is storage-type(%src<commits>), 'Hash', 'same through the original hash';

my sub raku-of(@a) { @a.raku }
is raku-of(%h.AT-KEY('a')), '[1, 2]', 'the bound array is not itemized';
