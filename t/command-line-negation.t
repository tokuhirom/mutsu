use Test;
use lib 'roast/packages/Test-Helpers/lib';
use Test::Util;

plan 3;

is_run Str, :args['-/h', '-e', 'say q[hi]'],
    {
        out => "hi\n",
        err => "",
        status => 0,
    },
    'negated short boolean launcher option is accepted';

is_run Str, :args['-/hv'],
    {
        out => rx/"SORRY" .+ "cannot be negated"/,
        err => '',
    },
    'negated grouped short launcher options are rejected';

is_run Str, :args['--/target', 'foo'],
    {
        out => rx/"SORRY" .+ "cannot be negated"/,
        err => '',
    },
    'negating a value-taking style launcher option is rejected';
