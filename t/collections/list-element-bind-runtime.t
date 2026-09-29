use Test;
use lib 'roast/packages/Test-Helpers/lib';
use Test::Util;

plan 5;

is_run(
    'say "start"; (1,2,3)[0] := 0;',
    {
        status => sub { $^status != 0 },
        out    => "start\n",
        err    => rx/'Cannot use bind operator with this left-hand side' .* 'in block <unit> at' .* 'line 1'/,
    },
    'an invalid List element bind fails at run time with a source location',
);

my $calls = 0;
try { (1, 2, 3)[$calls++] := $calls++ }
is $calls, 2, 'the index and right-hand side are evaluated before the bind fails';
isa-ok $!, X::Bind, 'the failure is an X::Bind exception';

throws-like { 10[0] := 1 }, X::Bind,
    'binding into an immutable scalar subscript also fails at run time';
throws-like { "Hi"[0] := 1 }, X::Bind,
    'binding into an immutable string subscript also fails at run time';
