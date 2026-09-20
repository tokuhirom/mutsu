# LWP::Simple parity measurement needs a network-test guard

LWP::Simple 0.109 was remeasured under the canonical bubblewrap harness after
PR #8538. The fixed `t/get-unsized.t` path remains green, but the distribution
is still partial at 5/8 baseline files and 42/46 assertions. The remaining
three baseline failures are network tests that conditionally require the
optional `IO::Socket::SSL` module: Rakudo skips them because the module is not
in the flat closure, while mutsu finds its intentional bundled SSL battery and
then reaches the sandbox's disabled network.

This measurement-contract gap is tracked in mutsu issue #8844. The distribution
was not modified.
