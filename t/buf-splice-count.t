use Test;

# Buf.splice(offset, count, replacement?) must honor the offset/count args
# (previously it ignored them and removed everything). Regression for the user
# IO::Handle `READ(\n) { $!data.splice: 0, n }` path in io-handle.t.

plan 10;

{
    my $b = Buf.new(1, 2, 3, 4, 5);
    is-deeply $b.splice.List, (1, 2, 3, 4, 5), 'no-arg splice removes all';
    is $b.elems, 0, '...and empties the Buf';
}

{
    my $b = Buf.new(1, 2, 3, 4, 5);
    is-deeply $b.splice(0, 1).List, (1,), 'splice(0,1) removes one element';
    is-deeply $b.List, (2, 3, 4, 5), '...leaving the rest';
}

{
    my $b = Buf.new(1, 2, 3, 4, 5);
    is-deeply $b.splice(1, 2).List, (2, 3), 'splice(1,2) removes a middle slice';
    is-deeply $b.List, (1, 4, 5), '...leaving the surrounding elements';
}

{
    my $b = Buf.new(1, 2, 3, 4, 5);
    is-deeply $b.splice(2).List, (3, 4, 5), 'splice(offset) removes to the end';
    is-deeply $b.List, (1, 2), '...leaving the prefix';
}

{
    my $b = Buf.new(1, 2, 3, 4, 5);
    is-deeply $b.splice(1, 2, Buf.new(9, 9, 9)).List, (2, 3),
        'splice with replacement returns the removed slice';
    is-deeply $b.List, (1, 9, 9, 9, 4, 5), '...and inserts the replacement';
}
