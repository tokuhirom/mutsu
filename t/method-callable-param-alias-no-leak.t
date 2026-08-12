use v6;
use Test;

# A Callable argument bound to a method's named parameter must not leak an
# `&name` alias into the caller's scope when the method returns. Text::CSV's
# `method csv(:$in)` receives `in => &provider`; the caller's own `sub in`
# must still be what a later `in()` call dispatches to (90_csv.t's in-format
# sweep re-enumerates inputs via `sub in` after every csv() call).
#
# The leak needed a nested method call: the callee frame's env (holding the
# `&in` param alias) was merged back into the caller env on return, and the
# merge kept every `&`-prefixed key unconditionally.

plan 6;

class Outer {
    method inner(Any :$in!) { 42 }
    method outer(Any :$in) { self.inner(:$in) }
    method outer-noargs(Any :$in) { self.inner(in => 1) }
}

sub in { "SUB IN" }

my $r = Outer.outer(in => -> { False });
is $r, 42, "nested method call with Callable named arg returns normally";
is in(), "SUB IN", "caller's sub in() still dispatches to the package sub";

my $r2 = Outer.outer-noargs(in => -> { "leak" });
is $r2, 42, "nested call passing a different arg returns normally";
is in(), "SUB IN", "outer param alias does not leak either";

# Positional Callable params leak the same alias mechanism.
class Pos {
    method inner($cb) { $cb() }
    method outer($cb) { self.inner($cb) }
}
sub cb { "SUB CB" }
is Pos.outer(-> { "arg cb" }), "arg cb", "positional Callable param dispatches inside the method";
is cb(), "SUB CB", "positional Callable param does not shadow the caller's sub";

done-testing;
