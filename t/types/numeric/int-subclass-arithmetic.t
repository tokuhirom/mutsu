use Test;

# EERPG's Amount type exposed arithmetic losing the payload of an Int subclass.
class Amount is Int { }

plan 4;
my $amount = Amount.new(5);

is $amount + 3, 8, 'arithmetic uses an Int subclass native payload';
is $amount - 2, 3, 'subtraction uses an Int subclass native payload';
is $amount * 2, 10, 'multiplication uses an Int subclass native payload';
ok $amount == 5, 'numeric comparison uses an Int subclass native payload';
