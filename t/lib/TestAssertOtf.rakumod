unit module TestAssertOtf;

# Helpers for t/test-assertion-module-otf.t.
#
# §D multi-dispatch VM-ization: an imported `is test-assertion` sub is now
# OTF-compiled to bytecode (previously the conservative gate kept it on the
# interpreter fallback). `call_compiled_function_named` pushes the test-assertion
# line context, so a failing assertion inside the helper still reports the
# caller's line.
use Test;

# A test-assertion helper. On failure Rakudo reports the CALLER's line (where
# `assert-eq(...)` was written), not the internal `ok` line.
sub assert-eq($got, $want, $desc) is export is test-assertion {
    ok($got eqv $want, $desc);
}

# A test-assertion helper with a default parameter (the name-cache-safe default
# case must also OTF-compile for an imported single sub).
sub assert-truthy($got, $desc = "is truthy") is export is test-assertion {
    ok(?$got, $desc);
}

# Returns the caller's source line (just used by the pin to show the helper
# composes; its exact value is still subject to the separate ORIGINAL_SOURCE
# clobber-on-`use` bug, fixed elsewhere).
sub caller-line is export { callframe(1).line }
