use Test;

# Binding a typed array to a plain `@a` parameter must not hand that element
# type to a variable of the same name in the caller. The light (positional)
# bind registered the parameter's constraint before the parameter was in
# scope, and so tagged the caller's `@a` -- a custom Positional instance, whose
# tag is process-wide -- with the argument's element type. Found via
# Concurrent::PriorityQueue t/01-basic (`for @a, @b, @c -> @a { @a.push(...) }`
# after Array::Sorted::Util's `insert(@a, $pos, \value)` ran on an `Int @!keys`).

plan 5;

sub ins(@a, $v) { @a.push($v) }

class P does Positional { method push($x) { "P.push $x" } }

my @a is P;
my Int @typed;
ins(@typed, 5);
is-deeply @typed, (my Int @ = 5), 'the typed argument got the element';
is @a.push("x"), 'P.push x', 'the caller\'s same-named @a is not constrained';
for @a -> @a {
    is @a.push("y"), 'P.push y', 'nor is a loop parameter bound to it';
}

throws-like { ins(@typed, "str") }, X::TypeCheck,
    'the parameter still enforces the argument\'s element type';

my @plain;
ins(@plain, "s");
is-deeply @plain, ["s"], 'an untyped argument takes anything';
