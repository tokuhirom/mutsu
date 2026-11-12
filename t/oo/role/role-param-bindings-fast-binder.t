use Test;

# A method of a parametric role sees the role's parameters (`T`), `::?CLASS`
# and its other composition bindings through whichever binder runs the call.
# The fast method binder used to refuse every such method; it now installs the
# same bindings the full binder does (#17331, found on Monad's
# `Monad::Maybe[::T]`).

plan 12;

role P[::T = Any] {
    method t { T }
    method tn { T.^name }
    method cls { ::?CLASS.^name }
    method typed(T $x) { $x }
    method echo($x --> T) { $x }
}

class DI does P[Int] { }
class DS does P[Str] { }

is DI.new.t.^name, 'Int', 'a role method reads T of its composition';
is DS.new.t.^name, 'Str', 'another composition binds its own T';
is DI.new.tn, 'Int', 'T.^name inside the method';
is DI.new.cls, 'DI', '::?CLASS is the composing class';
is DI.new.typed(42), 42, 'a T-typed parameter accepts a T';
throws-like { DI.new.typed('nope') }, X::TypeCheck::Binding,
    'a T-typed parameter rejects another type';
is DS.new.echo('x'), 'x', '--> T accepts a T';
throws-like { DS.new.echo(1) }, X::TypeCheck::Return, '--> T rejects another type';

# A role method calling the same role's method on another composition must not
# carry its own T back into the caller.
role Node[::T] {
    has $.child;
    method render { T.^name ~ ($!child ?? '(' ~ $!child.render ~ ')' !! '') }
}
class A does Node[Int] { }
class B does Node[Str] { }
is A.new(child => B.new(child => A.new)).render, 'Int(Str(Int))',
    'nested calls through different compositions keep their own T';

# The pun of a parametric role (its type object called directly).
is P[Rat].t.^name, 'Rat', 'a method on the punned role sees T';
my $loop = 0;
$loop += DI.new.t === Int for ^50;
is $loop, 50, 'repeated calls keep seeing T';

my $T = 'outer';
DI.new.t;
is $T, 'outer', "a caller's own \$T is untouched";
