use lib 't/lib';
use Test;

plan 6;

# `.wrap` on an imported `proto`/`multi` routine reaches the wrapper when a
# routine of another module calls it through its own import of the routine.
use WrapMultiCaller;
use WrapMultiFamily;

my $called = 0;
my $seen;
is named-call("a"), "named a", 'before the wrap: the multi runs';
&chat.wrap: -> |args { $called++; $seen = args<prompt>; "wrapped" };

is named-call("hello"), "wrapped", 'a named call from the module runs the wrapper';
is $called, 1, 'once';
is $seen, "hello", 'with the call arguments';
is positional-call("x"), "wrapped", 'a positional call from the module runs the wrapper';
is chat(prompt => "y"), "wrapped", 'a call from this unit runs the wrapper';
