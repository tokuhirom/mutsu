use Test;

# A with condition can be an lvalue with an effectful index. Evaluate the
# index once, while keeping the selected element as the writable topic.

plan 2;

my @values = 10, 20;
my $index = 0;
with @values[$index++] { $_ = $_ + 1 }

is $index, 1, 'an effectful with lvalue index is evaluated once';
is @values[0], 11, 'the with topic writes back to the selected element';
