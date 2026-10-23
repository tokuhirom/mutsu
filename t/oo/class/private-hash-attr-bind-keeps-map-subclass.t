use Test;

plan 6;

# `%!attr := Instance` aliases the Associative instance itself; it is not
# copied into a plain Hash, so the subclass and its methods survive.
my class FrozenMap is Map {
    method frozen-marker { 'frozen' }
}

my class Holder {
    has %.table;
    method TWEAK() { %!table := FrozenMap.new: (a => 1, b => 2) }
    method priv-name { %!table.^name }
}

my $h = Holder.new;
is $h.table.^name, 'FrozenMap', 'the accessor answers the bound subclass';
is $h.priv-name, 'FrozenMap', 'and so does the private attribute';
is $h.table.frozen-marker, 'frozen', 'its methods are reachable';
is-deeply $h.table<a b>, (1, 2), 'its entries are intact';
is Holder.^attributes[0].get_value($h).^name, 'FrozenMap', 'get_value answers it too';

my class Plain { }
my class BadHolder {
    has %.table;
    method TWEAK() { %!table := Plain.new }
}
throws-like { BadHolder.new }, X::TypeCheck::Binding, 'a non-Associative is still refused';
