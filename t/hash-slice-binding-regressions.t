use Test;

plan 5;

{
    my %h = 'foo' => [1,2,3], 'bar' => [4,5,6];
    is join(',', sort %h{*}».[1]), '2,5', 'hyper dot-index form works on whatever slice';
}

{
    my @a;
    @a[0,1] = 10,20;
    is ~@a, '10 20', 'slice assignment keeps comma RHS values together';
}

{
    my %h = :a(1), :b(2), :c(3), :d(4);
    my @slice := %h<b c>;
    (@slice,*) = <A B C D>;
    is ~@slice, 'A B', '(@slice,*) assignment respects bound slice width';
}

{
    my %h = :a<foo>, :b<bar>, :c<baz>;
    my $foo = 'FOO';
    my $bar = 'BAR';

    %h<a b> := ($foo, $bar);
    $foo = 'BB';
    $bar = 'CC';
    is ~%h<a b>, 'BB CC', 'hash slice bind reflects source variable changes';

    %h<a> = 'BBB';
    %h<b> = 'CCC';
    is "$foo $bar", 'BBB CCC', 'writes through bound hash slice update source variables';
}
