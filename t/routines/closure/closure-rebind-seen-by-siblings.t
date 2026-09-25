use Test;

# #9307: a `:=` rebind of an outer lexical made INSIDE a closure changes the
# outer binding, so the enclosing frame and every sibling closure over the
# same variable see it. A second name bound earlier (`my $f := $a`, #9207)
# keeps the old container.

plan 14;

sub sibling-sees-rebind {
    my $a = 1; my $b = 5;
    my &r = { $a };
    my &c = { $a := $b };
    c();
    is $a, 5, 'frame sees the rebind made in a closure';
    is r(), 5, 'sibling closure sees the rebind';
    $b = 6;
    is $a, 6, 'frame follows the new container';
    is r(), 6, 'sibling closure follows the new container';
}
sibling-sees-rebind();

sub alias-keeps-old {
    my $a = 1; my $b = 5;
    my $f := $a;
    my &r = { $a };
    my &c = { $a := $b };
    c();
    is "$a $f {r()}", '5 1 5', 'earlier alias keeps the old container';
    $f = 9;
    is "$a $f {r()}", '5 9 5', 'writing the old alias does not reach the rebound name';
}
alias-keeps-old();

{
    my $x = 1; my $y = 7;
    my &rx = { $x };
    my &cx = { $x := $y };
    cx();
    is rx(), 7, 'mainline: sibling closure sees the rebind';
    $y = 8;
    is "$x {rx()}", '8 8', 'mainline: both follow the new container';
}

sub nested-rebind {
    my $a = 1; my $b = 3;
    my &r = { $a };
    my &c = { my &d = { $a := $b }; d() };
    c();
    is "$a {r()}", '3 3', 'a rebind two closures deep reaches the declaring frame';
}
nested-rebind();

sub two-rebinders {
    my $a = 1; my $b = 5; my $c = 7;
    my &r = { $a };
    my &c1 = { $a := $b };
    my &c2 = { $a := $c };
    c1();
    is r(), 5, 'first rebinding closure';
    c2();
    is r(), 7, 'second rebinding closure';
    $a = 70;
    is "$b $c", '5 70', 'assignment after the rebinds writes the last-bound container';
}
two-rebinders();

sub per-iteration {
    my @out;
    for 1..3 -> $i {
        my $a = $i; my $b = $i * 10;
        my &r = { $a };
        my &c = { $a := $b };
        c(); $b++;
        @out.push: r();
    }
    is-deeply @out, [11, 21, 31], 'each iteration gets its own binding';
}
per-iteration();

sub rebind-to-literal {
    my $a = 1;
    my &r = { $a };
    my &c = { $a := 42 };
    c();
    is r(), 42, 'sibling sees a rebind to a literal';
}
rebind-to-literal();
