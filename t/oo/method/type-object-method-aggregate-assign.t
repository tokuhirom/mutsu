use Test;

# `Class.method = (...)` where the method (here a `:U` candidate forwarding to
# a singleton) returns a real Array: an Array IS a container, so this is a list
# assignment into it, as for a routine-call result. mutsu refused with
# "cannot assign through .dispatchers on non-instance". Reduced from
# Lumberjack's `Lumberjack.dispatchers = (...)`.

plan 3;

class LJ {
    my $inst;
    method !instance { $inst //= self.new }
    has @!dispatchers;
    has %!opts;
    proto method dispatchers(|) {*}
    multi method dispatchers(LJ:U:) { self!instance.dispatchers }
    multi method dispatchers(LJ:D:) { @!dispatchers }
    proto method opts(|) {*}
    multi method opts(LJ:U:) { self!instance.opts }
    multi method opts(LJ:D:) { %!opts }
}

LJ.dispatchers.append: 1;
LJ.dispatchers = (5, 6);
is-deeply LJ.dispatchers, [5, 6], 'list assignment into the returned Array';
LJ.opts = (a => 1);
is-deeply LJ.opts, {a => 1}, 'hash assignment into the returned Hash';
class P { method v { 42 } }
dies-ok { P.v = 1 }, 'a non-container result still refuses';
