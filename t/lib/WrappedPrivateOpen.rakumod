unit class WrappedPrivateOpen;

# A private (unexported) sub named like a core routine, in the `.wrap`ped state
# `is native` leaves every routine in (#16010).
sub open(Str $pathname, Int $flags) { "mine" }
&open.wrap(-> |c { callsame });

method hi { open("x", 1) }
