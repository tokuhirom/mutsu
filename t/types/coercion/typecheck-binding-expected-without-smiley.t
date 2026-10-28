use Test;

plan 4;

# Cookie::Jar (ecosystem): a wrong-typed argument for a `Str:D $key is copy`
# (or `is raw`) parameter names the nominal type only, as rakudo does.
class A {
    method copy(Str:D $key is copy) { 1 }
    method raw(Str:D $key is raw) { 1 }
    method plain(Str:D $key) { 1 }
}
sub f(Str:D $k is copy) { 1 }

for <copy raw plain> -> $m {
    throws-like { A.new."$m"(123) }, X::TypeCheck::Binding::Parameter,
        message => / 'expected Str but got Int' /, "method $m";
}
throws-like { f(123) }, X::TypeCheck::Binding::Parameter,
    message => / 'expected Str but got Int' /, 'sub with is copy';
