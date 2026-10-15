use Test;

plan 5;

# `is hidden-from-backtrace` on a `proto method` hides the proto's own
# dispatch frame, as it does for a multi candidate (Lumberjack's Logger role).

sub visible { Backtrace.new.list.grep({ !.is-hidden && !.is-setting }).map({ .subname }).join(",") }

class Cand { multi method log(Int $l) is hidden-from-backtrace { visible() }; method t { self.log(1) } }
is Cand.new.t, "visible,t,<unit>", 'a hidden multi candidate is left out';

class ProtoOnly {
    proto method log(|c) is hidden-from-backtrace { * };
    multi method log(Int $l) { visible() };
    method t { self.log(1) }
}
is ProtoOnly.new.t, "visible,log,t,<unit>", 'a hidden proto is left out, the visible candidate stays';

class Both {
    proto method log(|c) is hidden-from-backtrace { * };
    multi method log(Int $l) is hidden-from-backtrace { visible() };
    method t { self.log(1) }
}
is Both.new.t, "visible,t,<unit>", 'proto and candidate both hidden';

role R {
    proto method log(|c) is hidden-from-backtrace { * };
    multi method log(Int $l) is hidden-from-backtrace { visible() };
    method t { self.log(1) }
}
class Composed does R { }
is Composed.new.t, "visible,t,<unit>", 'a role proto composed into a class stays hidden';

class ImplicitProto {
    proto method log(|c) { * };
    multi method log(Int $l) { visible() };
    method t { self.log(1) }
}
is ImplicitProto.new.t, 'visible,log,t,<unit>', 'a {*} only proto method adds no extra backtrace frame';
