use Test;

# The three yada-yada stubs, measured on rakudo 2026.09: `...` fails (the
# caller gets a Failure), `!!!` dies on the spot, `???` warns and goes on.
# All three raise X::StubCode with the message, "Stub code executed" when
# none is written, and all three make the routine a stub (`.yada`).

plan 10;

sub fails { ... }
my $f = fails();
ok $f ~~ Failure, '`...` returns a Failure to its caller';
is $f.exception.^name, 'X::StubCode', 'carrying X::StubCode';
is $f.exception.message, 'Stub code executed', 'with the default message';

sub dies { !!! }
my $alive = True;
try { dies(); $alive = False };
ok $alive, '`!!!` dies instead of returning';
is $!.^name, 'X::StubCode', 'with X::StubCode';

sub dies-msg { !!! "boom" }
try dies-msg;
is $!.message, 'boom', '`!!! "msg"` carries the message';

sub warns { ??? }
my $warned;
{
    warns();
    CONTROL { when CX::Warn { $warned = .message; .resume } }
}
is $warned, 'Stub code executed', '`???` warns with the default message';

ok (sub { ... }).yada, '`...` makes a routine a stub';
ok (sub { !!! }).yada, 'so does `!!!`';
ok (sub { ??? }).yada, 'and `???`';
