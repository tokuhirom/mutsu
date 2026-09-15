use Test;

# A custom constructor passing |%args to bless must preserve the List
# container declared by an `is List` attribute rather than installing an
# itemized Array wrapper.

plan 3;

class WithList {
    has @.items is List;
    method new(*%args) { self.bless(|%args) }
}

my @items = 1, 2;
my %args = items => @items;
my $object = WithList.new(|%args);

is $object.items.^name, 'List', 'the declared List attribute keeps its type';
is $object.items.elems, 2, 'bless receives both List elements';
is $object.items.join(','), '1,2', 'the List attribute keeps its values';
