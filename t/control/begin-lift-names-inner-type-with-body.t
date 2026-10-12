use Test;

plan 3;

# A nested BEGIN that names a `my class` of its routine runs at compile time
# even though the class body runs code; the body itself runs only when the
# routine does.
my $log;
BEGIN $log = '';
sub f { my class K { $log ~= 'body;'; method m { 'meth' } }; BEGIN { $log ~= K.new.m ~ K.^name ~ ';' } }

is $log, 'methK;', 'BEGIN saw the class and its method before any call, without running the body';
f();
is $log, 'methK;body;', 'the class body ran with the call';
f();
is $log, 'methK;body;body;', 'and again on the next call, the BEGIN not repeated';
