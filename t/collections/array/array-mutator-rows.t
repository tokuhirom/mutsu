use Test;

plan 4;

# pop/shift on an empty typed array name the container as Array[T] (#12242)
my Int @t;
is @t.pop.exception.message, 'Cannot pop from an empty Array[Int]', 'pop on empty Array[Int]';
is @t.shift.exception.message, 'Cannot shift from an empty Array[Int]', 'shift on empty Array[Int]';
my @u;
is @u.pop.exception.message, 'Cannot pop from an empty Array', 'pop on empty untyped Array';
my Str @s;
is @s.shift.exception.message, 'Cannot shift from an empty Array[Str]', 'shift on empty Array[Str]';
