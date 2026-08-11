use Test;

# ADR-0025 slice 1: cell boxing of captured scalars is value-kind-blind —
# an Instance-holding captured-and-mutated scalar gets a shared cell just
# like a Str/Int one, so the capture neither loses to a same-named lexical
# in the calling chain (hijack) nor freezes the creation-time value
# (staleness).

plan 6;

class Enc { has $.v }

# 1) Hijack direction: the capture must win over a same-named env entry in
#    the CALLING closure's chain (the Cro::HTTP2 $encoder shape).
{
    my $enc;
    $enc = Enc.new(v => "MAIN");
    my @checks = ({ $enc.v },);
    my $got;
    sub invoke-a(@c, $out is rw) {
        my $runner = {
            my $enc = Enc.new(v => "CALLER");
            my $keep = { $enc };
            $out = @c[0]();
        };
        $runner();
    }
    invoke-a(@checks, $got);
    is $got, "MAIN", 'Instance capture wins over same-named Instance in caller chain';
}

# 2) Same, but the caller-chain shadow is a plain Str: before the fix this
#    crashed with "No such method 'v' for invocant of type 'Str'".
{
    my $enc2;
    $enc2 = Enc.new(v => "MAIN");
    my @checks2 = ({ $enc2.v },);
    my $got2;
    sub invoke-b(@c, $out is rw) {
        my $runner = {
            my $enc2 = "CALLER";
            my $keep = { $enc2 };
            $out = @c[0]();
        };
        $runner();
    }
    invoke-b(@checks2, $got2);
    is $got2, "MAIN", 'Instance capture wins over same-named Str in caller chain';
}

# 3) Staleness direction (the http-session $fake-now shape): a worker
#    thread started BEFORE the creator's rebind must observe the rebind
#    through an attribute-stored closure.
{
    class Holder { has &.now }
    my $x = Enc.new(v => 1);
    my $h = Holder.new(now => { $x });
    my $ch = Channel.new;
    my $out = Channel.new;
    my $worker = start {
        react {
            whenever $ch -> $ping {
                $out.send($h.now.().v);
            }
        }
    }
    $ch.send(0);
    is $out.receive, 1, 'worker reads creation-time value before rebind';
    $x = Enc.new(v => 2);
    $ch.send(1);
    is $out.receive, 2, 'worker observes post-capture rebind of Instance-holding capture';
    $ch.close;
    await $worker;
}

# 4) Regression guard: a cell formed while the scalar held an Int keeps
#    working after an Instance is assigned into it (pre-existing state).
{
    my $y = 1;
    my &get = { $y };
    $y = Enc.new(v => 42);
    is get().v, 42, 'cell formed as Int carries a later-assigned Instance';
}

# 5) The merge-site liveness example must keep passing: a call-arg closure
#    over a later-mutated scalar reads the live value.
{
    my $s = 0;
    my @cb;
    @cb.push({ $s });
    $s = 42;
    is @cb[0](), 42, 'post-capture mutation stays visible to call-arg closure';
}
