use v6;
use Test;

# Email::Simple (ecosystem): `has @.headers; has %!headers;` (a public array and
# a private hash with one bare name). Binding the hash (`%!headers := %h`) must
# not replace the array when the method returns.
plan 8;

class C {
    has @.items;
    has %!items;

    submethod TWEAK() {
        my %h;
        %h.push(.[0].lc => .[1]) for @!items;
        %!items := %h;
    }

    method rebind()  { my %h = z => 26; %!items := %h; }
    method array()   { @!items.map(*.[0]).join(',') }
    method hash-keys() { %!items.keys.sort.join(',') }
    method in-method() { my %h = y => 25; %!items := %h; "{@!items.elems}|{%!items.keys.sort.join(',')}" }
}

my $c = C.new(items => [['A', 1], ['B', 2]]);
is $c.array, 'A,B', 'the array survives the TWEAK bind';
is $c.hash-keys, 'a,b', 'the bound hash is the private attribute';
is $c.items.elems, 2, 'the public accessor still returns the array';

$c.rebind;
is $c.array, 'A,B', 'the array survives a later bind';
is $c.hash-keys, 'z', 'the hash was rebound';
is $c.in-method, '2|y', 'both read correctly inside the binding method';
is $c.array, 'A,B', 'and after it';
is $c.hash-keys, 'y', 'the new hash stays bound';
