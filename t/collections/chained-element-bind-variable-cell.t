use Test;

# #18534: binding a variable through a chained subscript (`%g<p><q> := $x`)
# shares the variable's Scalar, whether the intermediate levels already exist
# or are autovivified by the bind -- as a single-level bind does.

plan 10;

{
    my $x = 5;
    my %g;
    %g<p><q> := $x;
    is %g<p><q>.VAR.^name, 'Scalar', 'the bound element is a Scalar';
    $x = 7;
    is %g<p><q>, 7, 'a write to the variable is seen through a vivified path';
    %g<p><q> = 9;
    is $x, 9, 'a write to the element reaches the variable';
}
{
    my $y = 1;
    my %d;
    %d<a><b><c> := $y;
    $y = 3;
    is %d<a><b><c>, 3, 'three levels, all vivified';
    %d<a><b><c> = 4;
    is $y, 4, 'and the element writes through';
}
{
    my $z = 1;
    my %e = a => {};
    %e<a><b> := $z;
    $z = 2;
    is %e<a><b>, 2, 'an existing intermediate still binds the Scalar';
    %e<a><b> = 6;
    is $z, 6, 'and writes through';
}
{
    my $w = 1;
    my @a = [0, 0], [0];
    @a[0][1] := $w;
    $w = 8;
    is @a[0][1], 8, 'an existing array path binds the Scalar';
}
{
    my @source = 1, 2;
    my %h;
    %h<a><b> := @source;
    @source.push(3);
    is %h<a><b>.elems, 3, 'binding a whole Array through a vivified path aliases it';
    dies-ok { %h<a><b> = 3 }, 'and does not invent a writable Scalar';
}
