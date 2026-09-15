use Test;

# JSON::Class declares object-hash attributes with anonymous subset key
# constraints. The empty attribute created by C.new must retain the key
# constraint separately from the value type so subscripting can resolve it.

plan 3;

class Keyed {
    has Str %.bla{subset :: of Str where any('ble', 'blob')};
}

my $object = Keyed.new;
$object.bla<ble> = 'bli';
is $object.bla<ble>, 'bli', 'a valid anonymous-subset key can be read';
is $object.bla.elems, 1, 'the object hash stores the value';
dies-ok { say $object.bla<nope> },
    'an invalid anonymous-subset key is rejected';
