use Test;

plan 2;

class GatherHolder {
    has Str @.items is rw;
}

my $holder = GatherHolder.new;
$holder.items = gather {
    take 'alpha';
    take 'beta';
};

is-deeply $holder.items.List, ('alpha', 'beta'),
    'assigning a gathered Seq through an aggregate accessor stores its items';
is-deeply $holder.clone.items.List, ('alpha', 'beta'),
    'the aggregate accessor value remains cloneable after assignment';
