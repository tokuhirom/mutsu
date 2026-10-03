# `my $*ERR` inside a sub no longer leaks to the caller (regression pin)

The 2026-10-03 report that a sub-local `my $*ERR` kept routing the caller's `note`
output no longer reproduces on `main`: the twin-spelling fix for dynamic scalars
(#11348) already keeps `$*ERR` local to the declaring routine. This adds the
missing regression test, `t/vm/scope/my-dynamic-err-scope.t`, covering the
stderr-capture idiom, a plain non-handle `my $*ERR = 42`, and a second capture.
