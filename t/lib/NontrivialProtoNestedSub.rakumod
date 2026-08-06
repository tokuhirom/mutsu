unit module NontrivialProtoNestedSub;

my @calls;

proto foo($x) is export {
    my sub helper() { @calls.push('helper'); "helped" }
    helper();
    {*}
}
multi foo(Int $x) is export { "int:$x" }
multi foo(Str $x) is export { "str:$x" }

sub call-count() is export { @calls.elems }
