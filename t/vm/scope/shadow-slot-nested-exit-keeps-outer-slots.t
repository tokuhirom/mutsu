use Test;

# A block shadowing a name that an enclosing block already shadows must, on
# exit, restore only the binding the name denotes afterwards — never re-seed a
# further-out binding of the same name with that value (#10856).

plan 9;

{
    my $x = 1;
    { my $x = 2; { my $x = 3 }; is $OUTER::x, 1, '$OUTER::x after a nested shadow exits'; }
}

{
    my $x = 1;
    {
        my $x = 2;
        {
            my $x = 3;
            is $OUTER::x, 2, 'innermost sees the middle binding';
            is $OUTER::OUTER::x, 1, 'innermost sees the outermost binding';
        }
        is $x, 2, 'middle binding restored after innermost exits';
        is $OUTER::x, 1, 'outermost binding untouched by innermost exit';
    }
    is $x, 1, 'outermost binding after both exit';
}

{
    my $x = 1;
    {
        my $x = 2;
        {
            my $x = 3;
            { my $x = 4 }
            is $OUTER::OUTER::x, 1, 'four levels: outermost untouched';
            is $OUTER::x, 2, 'four levels: middle untouched';
        }
        is $OUTER::x, 1, 'four levels: outermost after two exits';
    }
}
