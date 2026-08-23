use Test;

plan 8;

# Assignment expressions must leave exactly their value on the VM stack.
{
    sub show($a, $b) { "$a/$b" }
    my ($x, $y);
    is show(42, (($x, $y) = 3, 4)), '42/3 4',
        'nested list assignment does not steal a preceding argument';
    is (($x, $y) = (5, 6)).join(','), '5,6',
        'list assignment still returns its assigned list';
}

# A signature-less block has an implicit flattening *@_ when it uses @_.
{
    is { @_.elems }((1, 2, 3)), 3,
        'signature-less block flattens a List into @_';
    my @items = 4, 5, 6;
    is { @_.elems }(@items), 3,
        'signature-less block flattens an Array into @_';
    is { @_.elems }('abc'.comb), 3,
        'signature-less block flattens a Seq into @_';
    is { @_[0] }(9, 8), 9,
        'signature-less block keeps ordinary positional arguments';
}

# Destructuring declarations accept statement modifiers like scalar declarations.
{
    my $value = 0;
    my ($a, $b) = $_, $_ + 1 given 10;
    is "$a/$b", '10/11', 'given applies to a destructuring declaration';
    my ($c, $d) = 20, 21;
    is "$c/$d", '20/21', 'plain destructuring remains unchanged';
}
