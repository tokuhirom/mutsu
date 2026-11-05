use Test;

# A compound assignment to an element with a literal or plain-variable subscript
# is expanded without a temporary for the subscript; every other subscript is
# still evaluated once. Checked against rakudo.
plan 14;

{
    my %h;
    my $k = 'a';
    %h{$k} += 1;
    %h{$k} += 2;
    %h<b> += 5;
    is %h<a>, 3, 'a variable subscript accumulates';
    is %h<b>, 5, 'a literal subscript accumulates';
    is (%h{$k} += 10), 13, 'the expression value is the stored value';
}

{
    my @a = 1, 2, 3;
    my $i = 1;
    @a[$i] *= 10;
    @a[2] -= 1;
    is @a.join(','), '1,20,2', 'an array element, variable and literal subscripts';
}

{
    my %g = a => 1;
    my $j = 'a';
    %g{$j} += do { $j = 'z'; 4 };
    is-deeply %g.sort.map(*.kv.join('=')).List, ('a=5',), 'a right-hand side that rebinds the variable still meets the original subscript';
}

{
    my %h;
    my $n = 0;
    %h{$n++} += 5;
    is $n, 1, 'an impure subscript is evaluated once';
    is %h{0}, 5, '... and addresses the first key';
}

{
    my %h;
    my $k = 'x';
    %h{$k} //= 7;
    %h{$k} //= 9;
    is %h<x>, 7, '//= is unaffected';
    %h{$k} ||= 1;
    is %h<x>, 7, '||= keeps a true value';
}

{
    my %h = a => 'x';
    my $k = 'a';
    %h{$k} ~= 'y';
    %h<a> .= uc;
    is %h<a>, 'XY', '~= and .= on an element';
}

{
    my @a;
    my $i = 0;
    @a[$i] += 5;
    is @a[0], 5, 'an element that does not exist yet is created';
    my %deep;
    my $k = 'p';
    %deep{$k}{'q'} += 3;
    is %deep<p><q>, 3, 'a nested subscript autovivifies';
}

{
    my %h;
    my $s = 0;
    for ^5 -> $i { %h{$i % 2} += $i; $s += $i }
    is-deeply %h.sort.map(*.kv.join('=')).List, ('0=6', '1=4'), 'in a loop, an operator subscript still works';
    is $s, 10, 'and the loop ran to the end';
}
