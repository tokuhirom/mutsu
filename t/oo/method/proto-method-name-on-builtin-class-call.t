use Test;

# Found via Log::Dispatch: a user class declares `proto method add(|)`, and the
# program then calls `.add` on a built-in class (`$*TMPDIR.add(...)`, an
# IO::Path). The proto-method lookup read the registry and, while still holding
# that read guard, asked for the class's MRO -- which writes the registry when
# the MRO is not cached yet. A read -> write upgrade on a non-reentrant lock:
# debug builds panic, release builds hang forever.

plan 6;

class Dispatcher {
    proto method add(|) {*}
    multi method add(Int:D $x) { "int $x" }
    multi method add(Str:D $s) { "str $s" }
}

my $t = $*TMPDIR.add("proto-method-name-{$*PID}");
isa-ok $t, IO::Path, 'IO::Path.add still reaches the built-in method';
is $t.basename, "proto-method-name-{$*PID}", 'and answers the joined path';

is Dispatcher.new.add(2), 'int 2', 'the user proto dispatches an Int candidate';
is Dispatcher.new.add('a'), 'str a', 'and a Str candidate';

is 'abc'.substr(1), 'bc', 'a built-in Str method call after the proto lookup';
is (1, 2, 3).List.elems, 3, 'and a built-in List method';
