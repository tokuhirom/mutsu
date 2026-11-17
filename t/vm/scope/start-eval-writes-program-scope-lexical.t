use Test;

# ADR-16595 slice 5 (#16600): a `start` block that names a program-scope
# lexical only reflectively (string `EVAL`, a symbolic `::('$x')`) writes the
# program's one binding, not the worker's copy of the scope.

plan 8;

my $x = 1;
await start { EVAL '$x = 42' };
is $x, 42, 'EVAL in a start block assigns the program-scope scalar';

my @a = 1;
await start { EVAL '@a.push(2)' };
is @a, [1, 2], 'EVAL in a start block pushes to the program-scope array';

my @b = 1;
await start { EVAL '@b = 5, 6' };
is @b, [5, 6], 'EVAL in a start block assigns the program-scope array';

my $y = 1;
await start { ::('$y') = 7 };
is $y, 7, 'a symbolic store in a start block reaches the program-scope scalar';

sub in-routine {
    my $z = 1;
    await start { EVAL '$z = 9' };
    $z
}
is in-routine(), 9, 'EVAL in a start block inside a routine assigns its lexical';

sub symbolic {
    my $w = 1;
    await start { ::('$w') = 3 };
    $w
}
is symbolic(), 3, 'a symbolic store inside a routine reaches its lexical';

my $*D = 0;
await start { $*D = 5 };
is $*D, 5, 'a dynamic written in a start block is the caller\'s container';

my $after = 1;
my $p = start { EVAL '$after' };
$after = 2;
ok (await $p) == 1 | 2, 'a reflective read sees the one binding';
