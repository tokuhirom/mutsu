use Test;

# A program that never names the same-method re-dispatch builtin pushes only
# the routine name as the method-call context; the one remaining reader is the
# parameter-binding error message, which must still name the method.

plan 4;

class C {
    method m(Int:D $x) { $x }
    multi method k(Int $x) { "int" }
    multi method k(Str $x) { "str" }
    method nested($n) { $n > 0 ?? self.nested($n - 1) !! 'done' }
}

my $o = C.new;
throws-like { $o.m(Int) }, X::Parameter::InvalidConcreteness,
    'type object for a :D parameter names the method';
try $o.m(Int);
like $!.message, /"routine 'm'"/, 'the message carries the method name';
is $o.k(1) ~ $o.k("a"), 'intstr', 'multi method dispatch is unaffected';
is $o.nested(5), 'done', 'recursive method calls push and pop their contexts';
