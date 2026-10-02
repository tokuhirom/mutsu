use Test;

# A multi-level element store autovivifies its inner levels whatever its root
# is: a parenthesized variable, a variable reached through `OUTER::` past a
# shadow, or an expression yielding a container. With an expression root the
# inner levels used to be read as rvalues, so the store landed in a throwaway
# and the container stayed empty (#10900).

plan 10;

{
    my %h;
    (%h)<a><b> = 1;
    is-deeply %h, {a => {b => 1}}, '(%h)<a><b> = v autovivifies';
}

{
    my @a;
    (@a)[0][1] = 1;
    is-deeply @a, [[Any, 1],], '(@a)[0][1] = v autovivifies';
}

{
    my %h;
    (%h)<a><b><c> = 3;
    is-deeply %h, {a => {b => {c => 3}}}, 'three levels';
}

{
    my %h = a => {b => 1};
    (%h)<a><b> = 5;
    is-deeply %h, {a => {b => 5}}, 'an existing inner level is stored into';
}

{
    my %h;
    my $r = %h;
    ($r)<x><y> = 1;
    is-deeply %h, {x => {y => 1}}, 'a parenthesized scalar holding a Hash';
}

{
    my %h;
    { my %h; %OUTER::h<a><b> = 1 }
    is-deeply %h, {a => {b => 1}}, '%OUTER::h<a><b> = v past an inner %h';
}

{
    my @a;
    sub s1 { my @a; @OUTER::a[0][1] = 2 }
    s1();
    is-deeply @a, [[Any, 2],], '@OUTER::a[0][1] = v past the sub\'s own @a';
}

{
    my %store;
    sub store { %store }
    store()<k><v> = 7;
    is-deeply %store, {k => {v => 7}}, 'a call returning a Hash';
}

{
    my Int @a;
    sub s2 { my @a; @OUTER::a[0][1] = "x" }
    throws-like { s2() }, X::TypeCheck,
        message => /'@a[0]'/, 'a type error names the variable, not its OUTER:: spelling';
}

{
    my @a = [1, 2], [3, 4];
    (@a)[1][0] = 9;
    is-deeply @a, [[1, 2], [9, 4]], 'an existing nested Array is stored into';
}
