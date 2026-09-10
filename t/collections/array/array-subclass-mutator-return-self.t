use Test;

plan 12;

class ArraySubclass is Array { }

{
    my $array = ArraySubclass.new(2);
    my $result = $array.push(1);

    ok $result === $array, 'push returns the invocant';
    is $result.^name, 'ArraySubclass', 'push preserves the subclass type';
    is-deeply $array.List, (2, 1), 'push still mutates the backing storage';
}
{
    my $array = ArraySubclass.new(2);
    my $result = $array.append(1);

    ok $result === $array, 'append returns the invocant';
    is $result.^name, 'ArraySubclass', 'append preserves the subclass type';
    is-deeply $array.List, (2, 1), 'append still mutates the backing storage';
}
{
    my $array = ArraySubclass.new(2);
    my $result = $array.prepend(1);

    ok $result === $array, 'prepend returns the invocant';
    is $result.^name, 'ArraySubclass', 'prepend preserves the subclass type';
    is-deeply $array.List, (1, 2), 'prepend still mutates the backing storage';
}
{
    my $array = ArraySubclass.new(2);
    my $result = $array.unshift(1);

    ok $result === $array, 'unshift returns the invocant';
    is $result.^name, 'ArraySubclass', 'unshift preserves the subclass type';
    is-deeply $array.List, (1, 2), 'unshift still mutates the backing storage';
}
