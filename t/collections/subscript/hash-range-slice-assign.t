use Test;

plan 8;

# Hash slice assignment with range index
{
    my %h;
    %h{^3} = (0 xx 3);
    is %h.elems, 3, '%h{^3} creates 3 entries';
    is %h{"0"}, 0, 'key "0" has value 0';
    is %h{"1"}, 0, 'key "1" has value 0';
    is %h{"2"}, 0, 'key "2" has value 0';
}

# Hash slice assignment with inclusive range
{
    my %h;
    %h{1..3} = (10, 20, 30);
    is %h.elems, 3, '%h{1..3} creates 3 entries';
    is %h{"1"}, 10, 'key "1" has value 10';
    is %h{"2"}, 20, 'key "2" has value 20';
    is %h{"3"}, 30, 'key "3" has value 30';
}
