use Test;

plan 8;

my $pipe = gather { take |(1, 2) };
is $pipe.elems, 1, 'take with a pipe produces one gathered item';
is $pipe[0].raku, '(1, 2)', 'the pipe arguments are bundled into one List';
is-deeply $pipe[0].list, (1, 2), 'the bundled List keeps both positional values';

my $slip = gather { take (1, 2).Slip };
is $slip.elems, 2, 'take of an explicit Slip still flattens';
is-deeply $slip.list, (1, 2), 'the explicit Slip produces separate gathered items';

my $looped = gather for 1 .. 10 -> $a, $b { take |($a, $b) };
is $looped.elems, 5, 'each piped take contributes one item';
is $looped[0].raku, '(1, 2)', 'the first looped piped take is bundled';
is $looped[4].raku, '(9, 10)', 'the last looped piped take is bundled';
