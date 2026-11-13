use Test;

# A role method sees its role's type parameters wherever it names them: in the
# body, in a parameter or return type, through a closure, in an attribute's
# type check -- and a method that names none of them still behaves the same
# (mutsu skips installing bindings such a call cannot observe, #17331).

plan 21;

role R[::T = Any] {
    has T $.value;
    method body-t { T }
    method smart($x) { $x ~~ T }
    method typed(T $y) { $y }
    method ret($v --> T) { $v }
    method arr { Array[T].new.^name }
    method closure { -> { T } }
    method set($v) { $!value = $v; self }
    method plain { 42 }
    method calls-plain { self.plain + 1 }
    method value-name { $!value.^name }
    method lexical { my T $z = $!value; $z }
    method sym { ::('T') }
}

my $r = R[Int].^pun.new(value => 3);
is $r.body-t.^name, 'Int', 'T in the body';
ok $r.smart(5), 'smartmatch against T';
nok $r.smart('x'), 'smartmatch against T rejects';
is $r.typed(7), 7, 'a T-typed parameter accepts';
throws-like { $r.typed('x') }, X::TypeCheck::Binding, 'a T-typed parameter rejects';
is $r.ret(9), 9, 'a T return type accepts';
throws-like { $r.ret('x') }, X::TypeCheck::Return, 'a T return type rejects';
is $r.arr, 'Array[Int]', 'Array[T] in the body';
is $r.closure()().^name, 'Int', 'T through a closure';
is $r.set(11).value, 11, 'an attribute store type-checks against T';
throws-like { $r.set('x') }, X::TypeCheck::Assignment, 'and rejects a non-T';
is $r.plain, 42, 'a method naming no binding';
is $r.calls-plain, 43, 'a method calling one';
is $r.value-name, 'Int', 'reading a T-typed attribute';
is $r.lexical, 11, 'a T-typed lexical';
is $r.sym.^name, 'Int', 'a symbolic ::("T") lookup';

my $s = R[Str].^pun.new(value => 'a');
is $s.body-t.^name, 'Str', 'another parameterization sees its own T';
nok $s.smart(5), 'and checks against it';
is $s.plain, 42, 'its plain method too';

class C does R[Rat] { }
is C.new.body-t.^name, 'Rat', 'a class composing the role sees its T';
is C.new.plain, 42, 'and its plain method';
