use Test;

# EERPG's Inventory exposed this role-punned associative path. A role-punned
# Associative object can delegate object-keyed lookups to an
# is-BagHash attribute. The subscript must preserve the object key instead of
# reducing it to a string before calling AT-KEY.

plan 4;

class Commodity {
    has $.name;
}

role InventoryLike does Associative {
    has %.stock is BagHash handles <AT-KEY ASSIGN-KEY EXISTS-KEY elems>;
}

my $food = Commodity.new(:name<Food>);
my $drink = Commodity.new(:name<Drink>);
my $inventory = InventoryLike.new(stock => :{ $food => 42, $drink => 666 });

is $inventory{$food}, 42, 'an object-keyed associative subscript reads the first value';
is $inventory{$drink}, 666, 'an object-keyed associative subscript reads the second value';
ok $inventory{$food}:exists, 'object-keyed associative existence preserves the key';
is $inventory.elems, 2, 'the delegated BagHash retains both object keys';
