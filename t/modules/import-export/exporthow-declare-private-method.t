use lib 't/lib';
use Test;

# A DECLARE'd HOW with its own `add_method` (OO::Monitors) re-adds every
# public method through the native `Metamodel::ClassHOW::add_method`. mutsu
# keys a private `method !name` under the same name, and the re-add replaced
# the name's whole candidate list, so a class declaring both `method stop` and
# `method !stop` (Timer::Stopwatch) lost `self!stop`.

use DeclareMonitorish;

plan 4;

traced Stopwatchish {
    method stop()  { self!stop; 'public' }
    method reset() { self!stop }
    method !stop() { 'private' }
}

my $s = Stopwatchish.new;
is $s.reset, 'private', 'the private method survives the public one being re-added';
is $s.stop, 'public', 'the public method still dispatches';
my $attr = Stopwatchish.^attributes.first(*.name eq '$!TRACE-log');
like $attr.get_value($s), /'stop;'/, 'the public method was wrapped by add_method';

traced PrivateFirst {
    method !go() { 'private' }
    method go()  { self!go }
}
is PrivateFirst.new.go, 'private', 'declaration order does not matter';
