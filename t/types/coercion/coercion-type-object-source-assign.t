use Test;

plan 5;

# A bare coercion type `T()` coerces from `Any`, so an undefined source (a
# type object) is handed to `T.COERCE` on assignment, as on parameter binding
# (PDF's `my PDF::COS::Null() $null = Any`).
class N {
    method defined { False }
    multi method COERCE(Any:U) is default { self.new }
}
my N() $a = Any;
isa-ok $a, N, 'declaration with an Any type object source';
my N() $b;
$b = Int;
isa-ok $b, N, 'later assignment of a type object';
sub f(N() $x) { $x }
isa-ok f(Str), N, 'parameter binding (unchanged)';

throws-like { my Int() $x = Str }, Exception, 'no coercion found still dies';

my Int() $i = '42';
is $i, 42, 'a defined source still coerces';
