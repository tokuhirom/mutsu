use v6;
use Test;

plan 3;

# `Scheduler` is a composable built-in role in Raku; a class may `does Scheduler`
# and supply its own scheduling methods. mutsu previously registered Scheduler as
# a plain class, so `class X does Scheduler {...}` failed with
# "Scheduler is not composable, so X cannot compose it" (seen loading the
# Test::Scheduler dist: `class Test::Scheduler does Scheduler {...}`).

class MyScheduler does Scheduler {
    method cue(&code, :$at, :$in, :$every, :$times = 1, :&stop, :&catch) { Nil }
    method loads() { 0 }
    method uncaught_handler is rw { my $h; $h }
}

my $s = MyScheduler.new;
ok $s.defined, 'a class doing Scheduler instantiates';
ok $s ~~ Scheduler, 'the instance smartmatches the Scheduler role';
ok MyScheduler ~~ Scheduler, 'the type object smartmatches the Scheduler role';
