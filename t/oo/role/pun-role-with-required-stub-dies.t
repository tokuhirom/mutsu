use v6;
use Test;

# Punning a role is a composition, so a role that still requires a method
# (`method m { ... }`) cannot be punned: Rakudo refuses `R.new` with
# "Method 'm' must be implemented by R because it is required by roles: R."
# mutsu used to pun it anyway and only die with "Stub code executed" once the
# stub was called. Regression pin for the JobQueue distribution, whose
# t/04-injection.rakutest checks `JobQueue::EventSink.new.dispatch('x')` dies
# with a message matching /'must be implemented'/.

plan 6;

role R {
    method m(Str:D $x, *%rest) { ... }
}

throws-like { R.new }, X::Comp::AdHoc,
    message => "Method 'm' must be implemented by R because it is required by roles: R.",
    'punning a role with a required stub dies at pun time';
throws-like { R.new.m('x') }, Exception, message => /'must be implemented'/,
    'the method is never reached';
ok R.HOW.^name.contains('Role'), 'the failed pun leaves R a role';

class C does R { method m(Str:D $x, *%rest) { "C:$x" } }
is C.new.m('y'), 'C:y', 'a class implementing the stub still composes';

role S { method m { 'concrete' } }
is S.new.m, 'concrete', 'a role without stubs still puns';

throws-like { R.new }, X::Comp::AdHoc, message => /'must be implemented'/,
    'the failed pun is not cached: a second attempt dies too';
