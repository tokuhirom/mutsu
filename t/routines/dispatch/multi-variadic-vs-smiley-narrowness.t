use Test;

# A slurpy positional that receives no argument does not make a candidate
# wider: rakudo compares only the positional parameters both candidates
# declare, and consults slurpiness only once those are tied (#11045).

plan 15;

{
    multi a(Str:D $n, *@p) { "S" }
    multi a(Any:D $r)      { "A" }
    is a('c'), 'S', 'Str:D + slurpy beats wider Any:D';
    is a(42), 'A', 'Any:D still takes a non-Str';
}

{
    multi c(Str $n, *@p) { "S" }
    multi c(Any:D $r)    { "A" }
    is c('c'), 'S', 'Str + slurpy beats wider Any:D';
}

{
    multi i(Int $x, *@r) { "IS" }
    multi i(Numeric $x)  { "N" }
    is i(1), 'IS', 'narrower shared type wins over a non-slurpy sibling';
}

{
    multi f($x)   { "x" }
    multi f(*@a)  { "slurpy" }
    is f(1), 'x', 'non-slurpy beats slurpy that swallows the argument';
    is f(), 'slurpy', 'slurpy takes the empty call';
}

{
    multi h(*@a) { "slurpy" }
    multi h($x)  { "x" }
    is h(1), 'x', 'non-slurpy wins regardless of declaration order';
}

{
    multi g(*@a)    { "slurpy" }
    multi g(Int $x) { "int" }
    is g(1), 'int', 'non-slurpy wins when declared second';
}

{
    multi j($x, *@r)  { "S" }
    multi j($x, $y)   { "XY" }
    is j(1, 2), 'XY', 'arity match beats slurpy';
}

{
    multi k(*@a)  { "S" }
    multi k(:$x)  { "N" }
    is k(:x), 'N', 'slurpiness is consulted before the named bind check';
}

{
    multi l(Int $x)       { "plain" }
    multi l(Int $x, *@r)  { "slurpy" }
    is l(1), 'plain', 'equal shared types: non-slurpy is narrower';
}

{
    class C {
        multi method url-for(Str:D $name, *@positional, *%named) { "name" }
        multi method url-for(Any:D $record, *%named)             { "record" }
    }
    is C.url-for('cart'), 'name', 'method multi: Str:D + slurpy beats Any:D';
    is C.url-for(C.new), 'record', 'method multi: Any:D takes an object';
}

{
    class M {
        multi method m(Int $a, *@r) { "slurpy" }
        multi method m(Int $a)      { "plain" }
        multi method n(Int $a, *@r) { "IS" }
        multi method n(Numeric $a)  { "N" }
    }
    is M.m(1), 'plain', 'method multi: equal shared types, non-slurpy wins';
    is M.n(1), 'IS', 'method multi: narrower shared type wins over non-slurpy';
}
