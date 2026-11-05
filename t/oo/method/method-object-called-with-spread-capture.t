use v6;
use Test;

# A Method object (`^find_method`) called as `$method(|c)` with the invocant
# coming out of a Capture spread: the caller's variable arrives as its cell, and
# the method used to run with `self` = Nil. Seen in the JSON::RPC dist, whose
# server does `$candidate( |params )` for a notification.

plan 7;

class App {
    has Int $.count is rw;
    method who { self.defined ?? 'inst' !! 'type' }
    method two($x) { "two:$x:" ~ (self.defined ?? 'inst' !! 'type') }
    method notify($n) { $.count = $n }
}

my $a = App.new;
my $who = $a.^find_method('who');
my $two = $a.^find_method('two');
my $notify = $a.^find_method('notify');

sub via-capture(Method $m, |c) { $m(|c) }
sub via-slurpy(Method $m, *@p) { $m(|@p) }

is via-capture($who, $a), 'inst', 'invocant variable through a |c spread';
is via-capture($who, App.new), 'inst', 'invocant temp through a |c spread';
is via-capture($two, $a, 5), 'two:5:inst', 'with an extra argument';
is via-slurpy($who, $a), 'inst', 'through a slurpy array spread';
is $who($a), 'inst', 'direct call';
via-capture($notify, $a, 7);
is $a.count, 7, 'the method wrote to the invocant\'s attribute';
my $cap = \($a);
is $who(|$cap), 'inst', 'spread of an explicit Capture value';
