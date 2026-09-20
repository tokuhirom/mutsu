use Test;

plan 3;

is-deeply (1, 2).Seq.clone.List, (1, 2),
    'cloning a Seq preserves its elements';
is-deeply (3, 4).Slip.clone.List, (3, 4),
    'cloning a Slip preserves its elements';

my @values = 1;
@values.append: |(2, 3);
is-deeply @values, [1, 2, 3],
    'flattening a Slip into append preserves all values';
