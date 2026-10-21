use Test;

# The call path's native stack cost per Raku call decides how deep recursion
# gets before ADR-0100's headroom guard raises (#9924). Rakudo reaches 100000
# frames on this routine; a release mutsu reached ~38000 before the call path's
# cold code was moved out of line and ~100000 after, so 50000 is asserted: well
# past the old limit, with room for build-to-build frame-size noise. A debug frame is several
# times larger (see deep-recursion-raises-instead-of-aborting.t), so the depth
# is only asserted for a release binary.
plan 1;

sub f($n) { $n == 0 ?? 0 !! 1 + f($n - 1) }

if $*EXECUTABLE.Str.contains('/release/') {
    is f(50000), 50000, 'a release build recurses 50000 frames deep';
}
else {
    skip 'depth is a property of the build; only a release binary is held to it', 1;
}
