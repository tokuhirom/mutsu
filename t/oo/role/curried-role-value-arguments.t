use Test;

# Curried-role arguments that are values rather than type objects.
# Reduced from the MergeOrderedSeqs distribution, whose iterator role is
# `role MergeOrderedSeqs[$before = Less]` and is instantiated as
# `MergeOrderedSeqs[|($_ with $before)].new(...)`.

plan 12;

role R[$b = Less] {
    method b { $b }
}

# An enum value is the argument itself; it is not numified the way a
# positional subscript on an array numifies it (`@a[More]` is `@a[1]`).
is R[More].new.b.raku, 'Order::More', 'enum argument binds the enum value';
is R[Order::More].new.b.raku, 'Order::More', 'qualified enum argument binds the enum value';
is R[True].new.b.raku, 'Bool::True', 'Bool argument binds the Bool';
is R[More].^name, 'R[Order]', 'curried name spells the enum argument by its type';
my @a = <a b c>;
is @a[More], 'b', 'an enum still numifies as an array subscript';

# A Slip spreads into the argument list; an empty one passes no arguments.
is R[|()].^name, 'R', 'R[|()] is the role itself';
is R[|()].new.b.raku, 'Order::Less', 'R[|()] applies the parameter default';
is R[|(More,)].new.b.raku, 'Order::More', 'a one-element Slip passes its element';
my $none;
is R[|($_ with $none)].new.b.raku, 'Order::Less', 'empty `with` Slip applies the default';
my @two = 1, 2;
role P[$x, $y] { method sum { $x + $y } }
is P[|@two].new.sum, 3, 'a Slip of an array spreads into several arguments';

# Two different closures are two different parameterizations, even though
# they print alike: punning the second must not reuse the first's class.
my $first  = R[{ 1 }];
my $second = R[{ 2 }];
is $second.new.b.(), 2, 'second closure argument binds its own closure';
is $first.new.b.(), 1, 'first closure argument keeps its own closure';
