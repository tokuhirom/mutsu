use Test;

# The method-dispatch probe chain asks, on every method call on every instance,
# whether the receiver is a user `IO::Handle` subclass overriding `WRITE`/`READ`.
# That probe is gated on a monotonic, process-global latch recording whether any
# user `WRITE` or `READ` method has been declared anywhere, so an ordinary
# program pays one load instead of resolving the receiver's class name and
# walking its MRO.
#
# The latch is armed by the reverse-index hook every method-table mutator calls,
# so a `WRITE` installed through the metamodel AFTER calls have already been
# dispatched still arms it. This pins that: without it, a user handle declared
# late would silently lose its output to the native path.
# (t/io/custom-io-handle-write-read.t covers the declared-in-the-class-body case.)
plan 3;

my @written;

class Plain { method m() { "plain" } }

# Method calls made while the latch is unarmed. The probe must decline for
# these, which it would do anyway -- the point is that it declines cheaply.
my $p = Plain.new;
is $p.m(), "plain", "an ordinary method call dispatches with the latch unarmed";

class LateHandle is IO::Handle {
    has @.sink;
}

my $h = LateHandle.new;

# Arm the latch through the metamodel, after the call above.
my $write = method ($buf) { @written.push($buf.decode('utf8')); $buf.elems };
LateHandle.^add_method('WRITE', $write);

$h.print("hello");
is @written, ["hello"], "a WRITE added later is reached by the user handle path";

is $p.m(), "plain", "the plain class still dispatches once the latch is armed";
