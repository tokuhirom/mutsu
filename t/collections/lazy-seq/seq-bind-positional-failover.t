use Test;

plan 5;

throws-like { my @s := <a b c>.Seq }, X::TypeCheck::Binding,
    'a Seq cannot bind directly to an array variable';

my @cached := <a b c>.Seq.cache;
is @cached.^name, 'List', 'the List view of a Seq can bind to an array variable';

my $pulled = 0;
my $source := gather {
    for 1..10 {
        $pulled++;
        take $_;
    }
};

sub fifths(@items) {
    is @items.^name, 'List', 'a Seq binds to an array parameter as a List';
    @items[4]
}

is fifths($source), 5, 'indexed parameter access reaches the fifth Seq element';
is $pulled, 5, 'parameter binding and indexing pull only the needed prefix';
