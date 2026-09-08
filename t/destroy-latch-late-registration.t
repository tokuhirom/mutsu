use Test;

# The interpreter keeps a monotonic, process-global latch recording whether any
# user DESTROY submethod has been registered anywhere; instance death skips the
# whole queue-and-MRO-walk dance while it reads false. The latch is consulted at
# DROP time, not at construction, so a DESTROY installed AFTER instances of the
# class have already lived and died still fires for everything dying later.
# (t/destroy.t covers the ordinary declared-in-the-class-body case.)
plan 2;

my @events;

class Early { has $.x }

# Instances that live and die entirely before any DESTROY exists anywhere: the
# latch is unarmed here, so nothing is queued and nothing runs.
{ my $e = Early.new(x => 1); }
{ my $e = Early.new(x => 2); }
quietly $*VM.request-garbage-collection;
is-deeply @events, [], "no DESTROY registered anywhere yet: nothing fires";

# Arm the latch through the metamodel, after those deaths.
my $late = method DESTROY { @events.push("early") };
Early.^add_method('DESTROY', $late);

{ my $e = Early.new(x => 3); }
quietly $*VM.request-garbage-collection;
is-deeply @events, ["early"], "a DESTROY added later fires for later deaths";
