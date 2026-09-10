use Test;

# A `handles`-delegated method whose target class declares a `proto` must run
# that proto body (its `{*}` dispatches to a multi) even when the delegating
# object is reached through a *variable* invocant (`$obj.meth` / `self.meth`
# inside a method), which routes through the mut method-dispatch path. That path
# previously forwarded the delegation straight to a multi candidate, skipping the
# proto (Template::Mustache Logger.log filtering broke as a result).

plan 4;

my @log;

class Inner {
    proto method note(:$level = 'Info', |) {
        @log.push("proto:$level");
        return if $level eq 'Skip';
        {*}
    }
    multi method note(:$level = 'Info', *@msg) {
        @log.push("multi:$level:{@msg.join}");
    }
}

class Outer {
    has $.inner handles <note>;
    submethod TWEAK { $!inner //= Inner.new }
    method go-self  { self.note(:level<Run>, "x") }
}

my $o = Outer.new;

# Fresh-expression invocant (non-mut path): proto then multi.
Outer.new.note(:level<Run>, "a");
is @log, ["proto:Run", "multi:Run:a"], "fresh invocant runs proto";

# Variable invocant (mut path): proto must still run.
@log = [];
$o.note(:level<Run>, "b");
is @log, ["proto:Run", "multi:Run:b"], "variable invocant runs proto";

# `self.meth` inside a method (mut path via self): proto must run.
@log = [];
$o.go-self;
is @log, ["proto:Run", "multi:Run:x"], "self.meth inside a method runs proto";

# The proto can short-circuit (its `return` filters the multi out).
@log = [];
$o.note(:level<Skip>, "c");
is @log, ["proto:Skip"], "proto short-circuit is honored on the mut path";
