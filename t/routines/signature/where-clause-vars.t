use Test;

plan 5;

# where clause on variable declaration
{
    my $x where * > 0 = 5;
    is $x, 5, 'my $x where * > 0 = 5 works';
}

# where clause on has attribute
{
    class WhereAttr {
        has $.x where * > 0;
    }
    my $obj = WhereAttr.new(x => 42);
    is $obj.x, 42, 'has $.x where * > 0 parses correctly';
}

# subset where * (already worked, but verify)
{
    subset PosInt of Int where * > 0;
    ok PosInt ~~ PosInt, 'subset where * > 0 is a valid type';
}

# named array parameter :@l
{
    sub f(:@l) { @l.join(',') }
    is f(:l[1,2,3]), '1,2,3', 'named array param :@l works';
}

# named array parameter with multi dispatch matching
{
    sub g(:@items) { @items.elems }
    is g(:items[10,20,30]), 3, 'named array param :@items works';
}
