use Test;
use OO::Monitors;

plan 6;

# OO::Monitors wraps every monitor method with `-> \SELF, | { … callsame }`
# (installed from `MetamodelX::MonitorHOW.add_method`). The wrap-chain dispatch
# copied the wrapper's persisted closure-env overrides back into the caller's
# env for every name the caller also has — including `$_` and `self`, which are
# per-frame and must never propagate out of a call.

monitor M {
    has $.n = 1;
    method plain($p) { 1 }
    method topicalizing($p) {
        with $p {
            when Int { }
            default { }
        }
        1
    }
}

class Holder {
    has $.attr = 'kept';
    has $.mon = M.new;
    method run() {
        $!mon.plain('x');
        $!attr            # reading an attribute after the monitor call
    }
}

{
    my $m = M.new;
    $_ = 'OUTER';
    $m.plain('x');
    is $_, 'OUTER', 'a monitor method call does not clobber the caller topic';
}

{
    my $m = M.new;
    $_ = 'OUTER2';
    $m.topicalizing('x');
    is $_, 'OUTER2', 'nor does one whose body topicalizes';
}

{
    # `self` used to come back bound to the MetamodelX::MonitorHOW that
    # installed the wrapper, so the next `$!attr` read threw
    # "P6opaque: no such attribute".
    is Holder.new.run, 'kept', 'an attribute read after a monitor call still works';
}

# A plain (non-monitor) wrapped method was always fine — keep it covered.
class C {
    has $.v = 'c-attr';
    method go($p) { 1 }
}
C.^find_method('go').wrap: -> \SELF, | { callsame };
{
    my $c = C.new;
    $_ = 'OUTER3';
    $c.go('x');
    is $_, 'OUTER3', 'a hand-wrapped method still does not leak the topic';
    is $c.v, 'c-attr', 'and self is intact';
}

# The wrap chain must still propagate ordinary captured-variable writes.
{
    my $seen = 0;
    class W { method go() { 1 } }
    W.^find_method('go').wrap: -> \SELF, | { $seen = 42; callsame };
    W.new.go;
    is $seen, 42, 'a wrapper still writes back its captured lexicals';
}
