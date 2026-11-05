use Test;

# A closure's captured container is read through its declaration-site cell;
# the read must follow a chain of cells (a `:=` alias of a captured container)
# and see later writes.

plan 7;

sub f() {
    my @x = 1, 2, 3;
    my @y := @x;
    my $sum = 0;
    for ^3 { $sum += [+] (^3).map({ @y[$_] }) }
    $sum
}
is f(), 18, 'a closure reads a captured container through an alias';

sub g() {
    my @x = 1, 2, 3;
    my &read = { @x[$_] };
    my $a = read(1);
    @x[1] = 20;
    ($a, read(1))
}
is g().join(','), '2,20', 'a captured container shows later writes';

sub h() {
    my %h = a => 1;
    my &r = { %h<a> };
    %h<a> = 5;
    r()
}
is h(), 5, 'a captured hash shows later writes';

sub k() {
    my @x = ^4;
    my @out = (^4).map({ @x[$_] * 2 });
    @out.join(',')
}
is k(), '0,2,4,6', 'map block over a captured array';

sub m() {
    my @x = ^3;
    my @y := @x;
    my @z := @y;
    (^3).map({ @z[$_] + 1 }).join(',')
}
is m(), '1,2,3', 'a chain of two aliases';

my @top = 5, 6;
is (^2).map({ @top[$_] }).join(','), '5,6', 'a file-scope array';

sub n() {
    my @x = 1, 2;
    my $c = { @x.elems };
    @x.push(3);
    $c()
}
is n(), 3, 'a push after capture is visible to the closure';

# vim: ft=raku
