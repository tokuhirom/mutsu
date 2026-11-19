use Test;

# A Block whose tail subscripts a missing element hands back that element's
# container, which vivifies the entry on the first write through it and reads
# as Any everywhere else; a subscript bound on an undefined Scalar vivifies
# the Scalar itself (Rakudo's whence containers, #17953).

plan 14;

{
    my %h;
    my $c := (-> { %h<a> })();
    is %h.raku, '{}', 'binding a block tail on a missing key creates nothing';
    $c = 5;
    is %h.raku, '{:a(5)}', '... and a write through it vivifies the key';
}

{
    my @a;
    my $c := (-> { @a[2] })();
    $c = 5;
    is @a.raku, '[Any, Any, 5]', 'a positional block tail vivifies the element';
}

{
    my %h;
    is (-> { %h<a> })(), Any, 'the tail reads as Any';
    nok %h<a>:exists, '... and reading it creates nothing';
    is ((-> { %h<a> })() // 7), 7, '... and is undefined';
    my $v = (-> { %h<a> })();
    $v = 3;
    is %h.raku, '{}', 'an assignment copies the value, not the container';
}

{
    my $x;
    my $c := $x<a>;
    is $x.raku, 'Any', 'binding a subscript of an undefined Scalar leaves it Any';
    $c = 1;
    is $x.raku, '${:a(1)}', '... and a write through it vivifies the Scalar';
}

{
    my $x;
    my $c := $x[1];
    $c = 1;
    is $x.raku, '$[Any, 1]', 'a positional subscript vivifies an Array';
}

{
    my %hash;
    my $slot := (%hash, |<a b c>).reduce: -> $h is raw, $k { $h{$k} };
    is %hash.raku, '{}', 'an is raw reduce descent creates nothing by itself';
    $slot = 42;
    is %hash.raku, '{:a(${:b(${:c(42)})})}', '... and the write vivifies the whole path';
}

{
    my %h = a => { b => 1 };
    my $slot := (%h, |<a b>).reduce: -> $h is raw, $k { $h{$k} };
    $slot = 2;
    is %h<a><b>, 2, 'an is raw reduce descent writes an existing element';
}

{
    my %h;
    my $c := (-> $h is raw { $h<a> })(%h);
    $c = 3;
    is %h.raku, '{:a(3)}', 'an is raw parameter receiver hands back its element';
}
