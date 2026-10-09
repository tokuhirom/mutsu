use Test;

# The built-in `.new` constructors are one table keyed by class name (ADR-11276 §9.49):
# the interpreter's `dispatch_new` and the VM's native construct path read the same
# entry. One call per family, so a table entry that goes missing shows here.

plan 19;

is Version.new('1.2.3').gist, 'v1.2.3', 'Version';
is Duration.new(5).Num, 5, 'Duration';
is Rat.new(1, 4).Num, 0.25, 'Rat';
is Complex.new(1, 2).im, 2, 'Complex';
is Pair.new('k', 'v').value, 'v', 'Pair';
is Slip.new(1, 2).elems, 2, 'Slip';
is Set.new(<a b a>).elems, 2, 'Set';
is Bag.new(<a b a>).total, 3, 'Bag';
is Mix.new(<a b>).elems, 2, 'Mix';
is Buf.new(1, 2, 3).elems, 3, 'Buf';
ok Promise.new.status ~~ Planned, 'Promise';
ok Channel.new ~~ Channel, 'Channel';
ok Supplier::Preserving.new ~~ Supplier, 'Supplier::Preserving';
ok Lock::Async.new ~~ Lock::Async, 'Lock::Async';
is Junction.new('one', 1..6).Bool, False, 'Junction';
throws-like { Supply.new }, X::Supply::New, 'Supply';
is Proc::Async.new('echo', 'hi').command.join(' '), 'echo hi', 'Proc::Async';

# A user class that is named like a builtin skips the table.
my class Wrapper {
    has $.v;
    method new(|c) { callsame }
}
is Wrapper.new(v => 7).v, 7, 'a user class constructs through its own path';

# A user subclass of a builtin with its own `new` defers to the builtin constructor.
class Counted is Version {
    method new(|c) { nextsame }
}
is Counted.new('2.0').gist, 'v2.0', 'a subclass constructor defers to the builtin one';
