use Test;

# A definedness smiley on a typed @/% attribute constrains its elements, not
# the collection. Keyring uses the :U form with an initializer containing
# backend type objects; applying :U to the Array itself rejects that valid
# default during construction.

plan 4;

class Backend { }
class Memory is Backend { }
my @backends = Memory;

class KeyringLike {
    has Backend:U @.backend-priority = @backends;
}

my $keyring = KeyringLike.new;
is $keyring.backend-priority.elems, 1,
    'typed :U collection default keeps its elements';
is $keyring.backend-priority[0], Memory,
    'typed :U collection accepts a type-object element';

class Invalid {
    has Int:U @.items = 1;
}
dies-ok { Invalid.new }, 'typed :U collection rejects a defined element';

class PairStore {
    has %!values;
    method AT-KEY(Pair:D $key) { %!values{$key.key ~ ":" ~ $key.value} }
    method ASSIGN-KEY(Pair:D $key, $value) {
        %!values{$key.key ~ ":" ~ $key.value} = $value;
    }
}
my $store = PairStore.new;
$store{"attribute" => "label"} = "secret";
is $store{"attribute" => "label"}, 'secret',
    'Pair subscript reads through a user-defined AT-KEY';
