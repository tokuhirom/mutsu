use Test;

plan 16;

# A core class's own constructor is part of its method table, and every type
# finds `new` (its own, or the `Mu.new` it inherits) through `.^find_method` /
# `.^lookup`. `Proc::Async.^method_table<new>` was missing, so Test::Mock's
# `mocked(Proc::Async, ...)` never overrode `new`. Issue #9340.

for Proc::Async, IO::Path, Lock -> $type {
    ok $type.^method_table<new>:exists, "{$type.^name}.^method_table lists its own new";
    ok $type.^methods(:local).first(*.name eq 'new'), "{$type.^name}.^methods(:local) lists it too";
}

for Supplier, Channel -> $type {
    nok $type.^method_table<new>:exists, "{$type.^name} has no constructor of its own";
}

for Proc::Async, IO::Path, Lock, Supplier, Channel, Promise -> $type {
    ok $type.^find_method('new'), "{$type.^name}.^find_method('new') finds a constructor";
}

is Lock.^find_method('new')(Lock).^name, 'Lock', 'the found constructor can be called';
is Lock.^lookup('protect').name, 'protect', '.^lookup finds a core class\'s native method';
