use Test;

plan 11;

class HashSubclass is Hash { }

{
    my $hash = HashSubclass.new(a => 1);
    my $result = $hash.push: (b => 2);

    ok $result === $hash, 'push returns the invocant';
    is $result.^name, 'HashSubclass', 'push preserves the subclass type';
    is $hash<b>, 2, 'push still mutates the backing storage';
}
{
    my $hash = HashSubclass.new(a => 1);
    $hash.ASSIGN-KEY('b', 2);
    is $hash.^name, 'HashSubclass', 'ASSIGN-KEY preserves the subclass type';
    is $hash<b>, 2, 'ASSIGN-KEY mutates the backing storage';
}
{
    my $hash = HashSubclass.new(a => 1, b => 2);
    $hash.DELETE-KEY('a');
    is $hash.^name, 'HashSubclass', 'DELETE-KEY preserves the subclass type';
    nok $hash.EXISTS-KEY('a'), 'DELETE-KEY mutates the backing storage';
}
{
    # `$h<a>++` on a scalar-held instance mutates the backing storage.
    my $hash = HashSubclass.new(a => 1);
    $hash<a>++;
    is $hash<a>, 2, 'post-increment on a subscript mutates storage';
    $hash<a>--;
    is $hash<a>, 1, 'post-decrement on a subscript mutates storage';
}
{
    # Subscript assignment must not clobber the instance's class identity.
    my $hash = HashSubclass.new;
    $hash{'x'} = 1;
    is $hash.^name, 'HashSubclass', 'subscript assignment does not clobber the instance';
    is $hash<x>, 1, 'subscript assignment writes the backing storage';
}
