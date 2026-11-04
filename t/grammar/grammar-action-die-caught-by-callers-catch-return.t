use Test;

# From the Template::HAML distribution: a caller's `CATCH { default { return
# ... } }` handles an exception an action died with, and its `return` leaves
# the caller. The action's `die` happens in the middle of `.parse`, so that
# `return` used to be dropped as if it were the action's own and the parse went
# on to answer normally.

plan 4;

grammar G { token TOP { <a> }; token a { x+ } }
class Boom { method a($/) { die "boom" } }
class Own { method a($/) { return make 7 } }

sub guarded() {
    CATCH { default { return "caught: " ~ .message } }
    G.parse("xx", :actions(Boom.new));
    "no exception";
}
is guarded(), 'caught: boom', "the caller's CATCH sees the action's die and its return leaves the caller";

sub typed() {
    CATCH { when X::AdHoc { return "adhoc" } }
    G.parse("xx", :actions(Boom.new));
    "no exception";
}
is typed(), 'adhoc', 'a typed when-clause as well';

is G.parse("xx", :actions(Own.new))<a>.made, 7, "an action's own return is still its result";
throws-like { G.parse("xx", :actions(Boom.new)) }, X::AdHoc, message => /boom/,
    'without a CATCH the exception leaves .parse';
