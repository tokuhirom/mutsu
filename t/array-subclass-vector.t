use Test;
plan 19;

# `class ... is Array` subclass with a custom `new` that redispatches the
# positional args to the base Array construction via `nextwith(|@values)`.
# The instance must behave as a Positional/Array: indexing, list context,
# gist, hyper ops, and rw-accessor / index element mutation all delegate to
# the backing storage. (roast integration/advent2013-day08.t)
class Vector is Array {
    method new (*@values is copy, *%named) {
        @values[0] = %named<x> if %named<x> :exists;
        @values[1] = %named<y> if %named<y> :exists;
        @values[2] = %named<z> if %named<z> :exists;
        nextwith(|@values);
    }
    method x () is rw { self[0] }
    method y () is rw { self[1] }
    method z () is rw { self[2] }
    method magnitude () { sqrt [+] self »**» 2 }
    method subtract (@vec) { self.new( self »-« @vec ) }
}

my @vec := Vector.new(1, 2, 3);
is @vec.WHAT.gist, '(Vector)', 'binds to @-sigil as Positional';
is @vec[0], 1, 'index 0';
is @vec[2], 3, 'index 2';
is @vec.elems, 3, 'elems delegates to storage';

my @vec-wrong = Vector.new(1, 2, 3);
is @vec-wrong.WHAT.gist, '(Array)', 'assignment (not binding) flattens to Array';

my $vec := Vector.new(1, 2, 3);
is $vec.WHAT.gist, '(Vector)', 'scalar bind keeps Vector type';
is $vec.gist, '[1 2 3]', 'gist shows the elements';
is $vec.List.gist, '(1 2 3)', 'List delegates to storage';

my @position := Vector.new(1, 2);
my @destination = 4, 6;
is @position.subtract(@destination).magnitude, 5, 'hyper op + reduce chain';

{
    my $v = Vector.new(0, 1);
    is $v.magnitude, 1, 'magnitude of (0,1)';
}

# rw accessor returning self[N] is an assignable lvalue.
{
    my $v = Vector.new(1, 2);
    $v.z = 3;
    is $v.gist, '[1 2 3]', 'rw accessor self[2] assign autovivifies';
}

# named-arg construction path.
{
    my $v = Vector.new(:x(1), :y(2));
    $v.z = 3;
    is $v.gist, '[1 2 3]', 'named-arg construction + rw z';
}

# direct index assignment and post-inc/dec on the instance.
{
    my $v = Vector.new(1, 2, 3);
    $v[1] = 99;
    is $v.gist, '[1 99 3]', 'direct index assign writes storage';
    $v[1]--;
    is $v.gist, '[1 98 3]', 'index post-decrement writes storage';
    $v[0]++;
    is $v.gist, '[2 98 3]', 'index post-increment writes storage';
    $v.x++;
    is $v.gist, '[3 98 3]', 'rw accessor post-increment';
}

# list context iterates the elements, not the instance as one item.
{
    my $v = Vector.new(:y(3), 0);
    $v.x++;
    $v[1]--;
    $v.z = 3;
    my @seen;
    @seen.push($_) for @$v;
    is-deeply @seen, [1, 2, 3], 'for @$v iterates storage elements';
}

# push still works and grows storage.
{
    my $v = Vector.new(1, 2);
    $v.push(3);
    is $v.elems, 3, 'push grows storage';
    is $v[2], 3, 'pushed element readable';
}
