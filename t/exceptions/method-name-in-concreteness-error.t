use Test;

# The enclosing method's name reaches the concreteness error of a parameter or
# invocant check. The call frame keeps that name for the message; it must be the
# method that was called, also when the same method is called again and nested
# (ADR-14633: the name is borrowed from the interned method name).

plan 6;

class A {
    method m(A:D: $x) { $x }
    method n(Int:D $x) { $x }
    method outer($v) { self.n($v) }
}

try A.m(1);
my $e = $!;
isa-ok $e, X::Parameter::InvalidConcreteness, 'an invocant type object is refused';
like $e.message, /"method 'm'"/, 'the invocant error names the method';

my $a = A.new;
try $a.n(Int);
my $f = $!;
isa-ok $f, X::Parameter::InvalidConcreteness, 'a type-object argument is refused for :D';
like $f.message, /"routine 'n'"/, 'the parameter error names the routine';

try $a.outer(Int);
my $g = $!;
like $g.message, /"routine 'n'"/, 'a nested call names the inner routine, not the outer one';
is $a.n(5), 5, 'and a good call afterwards is unaffected';
