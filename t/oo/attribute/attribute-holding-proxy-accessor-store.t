use v6;
use Test;

# An attribute that holds a Proxy (bound there with Attribute.set_value, as Red
# does for every model column) is assigned through that Proxy: `$obj.attr = v`
# is its STORE. Found by RedX::HashedPassword (Red's dirty tracking).

plan 4;

class Box {
    has Str $.text is rw;
    has $.log is rw = 0;
}

my $box = Box.new;
my @stored;
my \proxy = Proxy.new(
    FETCH => method { "fetched" },
    STORE => method (\value) { @stored.push(value) },
);
Box.^attributes.first(*.name eq '$!text').set_value($box, proxy);

is $box.text, 'fetched', 'the accessor reads through the Proxy FETCH';
$box.text = 'x';
is-deeply @stored, ['x'], 'assigning through the accessor fires STORE';

# The STORE body may write the very instance that is being assigned.
my $self-writer = Box.new;
my \writer = Proxy.new(
    FETCH => method { "w" },
    STORE => method (\value) { $self-writer.log = 3 },
);
Box.^attributes.first(*.name eq '$!text').set_value($self-writer, writer);
$self-writer.log = 7;
$self-writer.text = 'y';
is $self-writer.log, 3, 'a STORE writing its own instance is not rolled back';
is $self-writer.text, 'w', 'the Proxy stays installed after the store';
