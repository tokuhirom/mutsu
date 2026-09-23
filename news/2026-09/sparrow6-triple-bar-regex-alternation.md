# Sparrow6 reaches ecosystem parity

Sparrow6 0.0.95 was blocked because mutsu rejected the `|||` spelling used by
`Sparrow6::RakuTask`'s file-extension regex as a null regex. The regex splitter
now consumes the three adjacent bars as a sequential-alternation separator,
while still rejecting genuinely empty branches.

Pinned by `t/regex/null-regex.t`. Sparrow6 moves from `blocked_load` to `green`:
4/4 baseline files and 4/4 assertions.

This distribution was selected by the ecosystem roulette and locked on
[#8977](https://github.com/tokuhirom/mutsu/issues/8977).
