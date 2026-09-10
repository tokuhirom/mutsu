use Test;
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test::Util;

plan 3;

{
    my $msg = 'die or fail without parameters';
    try { die $msg };
    is "$!", $msg, 'die with argument sets $!';
    try { die };
    is "$!", $msg, 'die with no argument uses $!';
}

is_run(
    'use Test; pass; die "uh-oh"',
    {
        status => sub { 0 != $^a },
        out    => rx/'ok 1 -'/,
        err    => rx/'uh-oh'/,
    },
    'pass output keeps TAP dash before die',
);
