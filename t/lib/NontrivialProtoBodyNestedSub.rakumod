unit module NontrivialProtoBodyNestedSub;

our proto sub labeled($x) is export {
    my sub helper($v) { return "helper($v)"; }
    say "before";
    {*}
    return helper($x);
}
multi sub labeled(Int $v) { say "int candidate: $v"; }
multi sub labeled(Str $v) { say "str candidate: $v"; }
