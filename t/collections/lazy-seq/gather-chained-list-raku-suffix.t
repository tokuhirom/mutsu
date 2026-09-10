use Test;

plan 8;

# A direct `.list` call on a finite gather must return a List view, just like
# the same call after the gather has been stored in a variable. In particular,
# `.raku` must not expose the gather's implementation-level Seq wrapper.
is (gather { }).list.raku, '()', 'empty gather.list is a List';
is (gather { take 1; take 2 }).list.raku, '(1, 2)', 'direct gather.list is a List';
is (gather { for 1, 2 { take $_ } }).list.raku, '(1, 2)',
    'finite for gather.list is a List';
is (gather {
    my $i = 0;
    while $i < 2 { take ++$i }
}).list.raku, '(1, 2)', 'finite while gather.list is a List';
is (gather { take 1; take 2 }).List.raku, '(1, 2)', 'direct gather.List is a List';
is (gather { take 1; take 2 }).values.raku, '(1, 2)', 'direct gather.values is a List';
is (gather { loop { take 1 } }).list[2], 1, 'infinite gather.list stays pullable';

my $g := gather { take 1; take 2 };
is $g.list.raku, '(1, 2)', 'stored gather.list is a List';
