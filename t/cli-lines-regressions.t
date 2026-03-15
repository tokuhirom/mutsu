use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

plan 2;

is_run(
    '1',
    "foo\n",
    { out => "foo\n", err => '', status => 0 },
    '-p wraps code in a line-printing loop',
    :compiler-args['-p'],
);

is_run(
    'print lines[0]',
    "abcd\nefgh\nijkl\n",
    { out => 'abcd', err => '', status => 0 },
    'lines[0] treats lines as a zero-arg builtin',
);
