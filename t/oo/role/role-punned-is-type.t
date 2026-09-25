use Test;

# EERPG's Inventory role declares its storage as AccountableBagHash.  A role
# instantiated directly must preserve that container trait after punning.

plan 4;

role InventoryLike {
    has %.stock is BagHash handles <AT-KEY ASSIGN-KEY elems>;
}

my $inventory = InventoryLike.new;
my $stock = $inventory.stock;
isa-ok $stock, BagHash, 'a punned role preserves its is BagHash container';
is $stock.elems, 0, 'the preserved container starts empty';

$inventory<food> = 42;
is $inventory<food>, 42, 'the preserved BagHash remains writable';
is $stock.WHAT, BagHash, 'the container keeps its concrete type';
