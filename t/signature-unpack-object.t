use Test;
plan 4;

{
    my $tracker = '';
    for a => 1, b => 2 -> Pair $p (:$key, :$value) {
        $tracker ~= "|$key,$value";
    }
    is $tracker, '|a,1|b,2', 'for-loop Pair unpacking binds key/value';
}

{
    class A { has $.x };
    my $tracker = '';
    for A.new(x => 4), A.new(x => 2) -> $ (:$x) {
        $tracker ~= $x;
    }
    is $tracker, '42', 'for-loop object unpacking binds public attribute reader';
}

{
    multi f((Int :$value, *%)) { "Int $value" }
    multi f((Str :$value, *%)) { "Str $value" }
    is f('a' => 3), 'Int 3', 'sub-signature dispatch chooses Int candidate';
    is f('a' => 'x'), 'Str x', 'sub-signature dispatch chooses Str candidate';
}
