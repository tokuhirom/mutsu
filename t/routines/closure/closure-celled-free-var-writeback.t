use Test;

plan 5;

# A closure's celled free variable is its identity: writes through the cell
# reach the caller without any writeback, and a rebinding is still seen.
sub f() {
    my @x = ^5;
    my $sum = 0;
    for ^3 { $sum += [+] (^5).map({ @x[$_] }) }
    $sum
}
is f(), 30, 'a closure reading a celled array sums correctly';

sub g() {
    my @x;
    (^5).map({ @x.push($_) });
    @x.join(',')
}
is g(), '0,1,2,3,4', 'a closure pushing to a celled array is seen by the caller';

sub h() {
    my %h;
    (^3).map({ %h{$_} = $_ * 2 });
    %h.sort.map(*.value).join(',')
}
is h(), '0,2,4', 'a closure storing into a celled hash is seen by the caller';

sub k() {
    my @x = 1, 2, 3;
    my &c = { @x = 7, 8 };
    c();
    @x.join(',')
}
is k(), '7,8', 'a closure reassigning a celled array is seen by the caller';

sub m() {
    my $n = 0;
    my @a;
    my &inc = { $n++; @a.push($n) };
    inc() for ^3;
    "$n:{@a.join(',')}"
}
is m(), '3:1,2,3', 'scalar and array captured together stay in step';
