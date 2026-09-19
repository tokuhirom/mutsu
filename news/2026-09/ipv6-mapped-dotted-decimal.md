# IPv4-mapped IPv6 grammar keeps its dynamic hextet limit

Grammar subrules with a temporary dynamic declaration now preserve that
declaration while ratcheted candidates backtrack through their continuations.
This lets IPv4-mapped IPv6 addresses with dotted-decimal suffixes parse as they
do in Rakudo.

Pinned by `t/grammar/ipv6-mapped-dotted-decimal.t`. Fixes #8753.
