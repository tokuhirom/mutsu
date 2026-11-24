use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

plan 10;

# rakudo's `X::AdHoc.message` is `$!payload.Str`: an object payload renders
# through its own (user) `Str` method, not as the object (#19083).
class F { method Str { "FF" } }

my $e = X::AdHoc.new(payload => F.new);
is $e.message, 'FF', 'message calls the payload object\'s Str';
is $e.Str, 'FF', 'Str too';
is "$e", 'FF', 'interpolation too';
is $e.gist.lines[0], 'FF', 'gist too';
isa-ok $e.payload, F, 'the payload is still the object';

my $f = Failure.new(F.new);
$f.so;
is $f.exception.message, 'FF', 'a Failure wrapping an object payload';

is_run 'class F { method Str { "FF" } }; X::AdHoc.new(payload => F.new).throw',
    { :out(''), :err(/^ 'FF' \n/) }, 'an uncaught one prints the Str';

# A Str that dies makes message die.
class D { method Str { die "boom" } }
throws-like { X::AdHoc.new(payload => D.new).message }, X::AdHoc, message => 'boom',
    'a dying Str propagates';

# A value payload is unchanged.
is X::AdHoc.new(payload => Date.new(2020, 1, 2)).message, '2020-01-02', 'a Date payload';
is X::AdHoc.new(payload => ('a', 'b')).message, 'a b', 'a list payload';
