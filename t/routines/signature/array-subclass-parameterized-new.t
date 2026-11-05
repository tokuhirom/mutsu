use Test;

# #16658 (found by Array::Rounded): a user subclass of Array, parameterized
# (`Foo[Str]`), keeps Array behaviour, checks and reports its element type,
# and `my Str @a is Foo` is a `Foo[Str]`.

plan 25;

class P is Array { method total { [+] self } }

my $x = P[Int].new(1, 2, 3);
is $x.elems, 3, 'P[Int].new keeps its three elements';
is $x.Int, 3, '.Int is the element count';
is $x[2], 3, 'elements are indexable';
is $x.total, 6, 'a user method of P sees the elements';
is $x.of.^name, 'Int', '.of is the element type';
is $x.^name, 'P[Int]', '.^name is P[Int]';
ok $x ~~ P, 'a P[Int] is a P';
ok $x ~~ Array, 'a P[Int] is an Array';
is $x.WHAT.gist, '(P[Int])', '.WHAT';

throws-like { $x.push("a") }, X::TypeCheck::Assignment, 'a push of the wrong type is refused';
$x.push(4);
is $x.elems, 4, 'a push of the right type works';
throws-like { P[Int].new(1, "b") }, X::TypeCheck::Assignment, 'the constructor checks its elements';
is P[Int].new(<5 6>).elems, 2, 'a one-argument Iterable spreads';
is P[Str].new.elems, 0, 'an empty P[Str]';
is P.new("a", "b", "c").elems, 3, 'the unparameterized class is unchanged';

my class Foo is Array { }
my Str @t is Foo = <a b>;
is @t.^name, 'Foo[Str]', 'my Str @t is Foo is a Foo[Str]';
is @t.of.^name, 'Str', 'with element type Str';
is @t.elems, 2, 'and its elements';
my @u is Foo = <a b c>;
is @u.^name, 'Foo', 'an untyped array is still a plain Foo';
is @u.elems, 3, 'with its elements';
my Int @n is Foo = 1, 2;
is @n.^name, 'Foo[Int]', 'my Int @n is Foo';
throws-like { my Int @bad is Foo = 1, "x" }, X::TypeCheck::Assignment, 'the initializer is type-checked';

is P[Str].new("a").^mro.map(*.^name).head(2).join(','), 'P[Str],P', 'the MRO starts P[Str], P';
ok P[Str].new("a") ~~ P[Str], 'a P[Str] matches its own type';
nok P[Int].new(1) ~~ P[Str], 'a P[Int] is not a P[Str]';
