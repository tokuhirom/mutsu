use Test;

# #11022: `%h{1}++` on an object hash must record the key object (an `Int`),
# also when a closure captured a same-named `%h` in a sibling block, which
# routes the increment through the shared-cell writeback.

plan 4;

{
    my %h;
    my &bump = -> $k { %h{$k}++ };
    bump('q') for ^4;
    is %h<q>, 4, 'the capturing closure still counts';
}
{
    my %h{Any};
    %h{1}++;
    is %h.keys[0].WHAT.gist, '(Int)', '++ keeps the Int key object';
    %h{2}++;
    is %h.keys.sort.map({ .WHAT.gist }).join(','), '(Int),(Int)', 'every key keeps its type';
    is %h{1}, 1, 'the element counted';
}
