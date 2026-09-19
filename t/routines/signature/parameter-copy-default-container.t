use Test;

plan 3;

my @defaults = <one two>;
sub mutate(:@items is copy = @defaults) {
    @items[0] = 'changed';
    @items.push('added');
    @items
}

my @first = mutate();
my @second = mutate();

is @first.^name, 'Array', 'an is-copy default array parameter is mutable';
is-deeply @first, ['changed', 'two', 'added'], 'the first call gets its own copy';
is-deeply @defaults, ['one', 'two'], 'mutating the default copy leaves the source unchanged';
