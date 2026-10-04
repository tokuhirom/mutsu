use Test;

# `C[T]` in a signature, for a `C` with its own `method ^parameterize`,
# denotes the type object that meta-method returns. `Parameter.type`,
# `Signature.returns` and `Routine.returns` answer that object, not a bare
# name, so its REPR and methods are visible (upstream NativeCall's
# `check_routine_sanity` reads `.REPR` and `.^can('of')` off them).

plan 6;

role Typed[::E] { method of { E } }
class Box is repr('CPointer') {
    method ^parameterize(Mu:U \b, Mu:U \t) {
        my $w := b.^mixin(Typed[t]);
        $w.^set_name("Box[{t.^name}]");
        $w
    }
}

sub f(Box[Int] $b --> Box[Str]) { }

my $type := &f.signature.params[0].type;
is $type.^name, 'Box[Int]', 'Parameter.type keeps the name';
is $type.REPR, 'CPointer', 'Parameter.type is the parameterized type object';
is $type.of.^name, 'Int', 'Parameter.type carries the mixed-in role';
is &f.signature.returns.REPR, 'CPointer', 'Signature.returns is the type object';
is &f.returns.REPR, 'CPointer', 'Routine.returns is the type object';
is &f.returns.of.^name, 'Str', 'Routine.returns carries the mixed-in role';
