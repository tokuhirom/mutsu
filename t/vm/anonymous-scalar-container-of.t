use Test;

# `.VAR` of an anonymous scalar (`(my $ = 5)`, `(my Int $ = 5)`) and of an
# item (`$(1, 2)`) is a Scalar container that answers `.of`, not the `Scalar`
# type object (#18349).

plan 12;

is (my $ = 5).VAR.of.^name, 'Mu', 'an untyped anonymous scalar is of Mu';
is (my $ = 5).VAR.^name, 'Scalar', 'and its .VAR is a Scalar';
ok (my $ = 5).VAR.defined, 'a container, not the Scalar type object';
is (my Int $ = 5).VAR.of.^name, 'Int', 'a typed anonymous scalar is of its type';
is (my Int $ = 5).VAR.^name, 'Scalar', 'and its .VAR is a Scalar';
is $(1, 2).VAR.of.^name, 'Mu', 'an item is of Mu';
is $(1, 2).VAR.^name, 'Scalar', 'and its .VAR is a Scalar';

{
    my @l = ((my $ = 1), 2);
    @l[0] = 9;
    is @l.join(' '), '9 2', 'an anonymous scalar in a list is still writable';
}

is (my $ = [1]).push(2).join(' '), '1 2', 'a method reaches the held array';
is (my Int $ = 5) + 1, 6, 'a typed anonymous scalar reads as its value';
is (my $ = 5).succ, 6, 'a value method reads through the container';
throws-like { (my Int $ = 5) = 'a' }, X::TypeCheck::Assignment,
    'the typed container still checks assignments';
