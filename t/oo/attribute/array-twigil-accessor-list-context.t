use Test;

plan 2;

class SequenceAccessor {
    method keys { (1, 2).Seq }
    method list-context { @.keys }
}

my $object = SequenceAccessor.new;
my $values = $object.list-context;

is $values.^name, 'List', '@.method coerces the accessor result to List context';
is $values.join(','), '1,2', 'the list-context accessor keeps the returned values';
