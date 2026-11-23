# TRIR's per-codepoint `Uni` ops run inline

JSON::Fast unescapes a string by shifting each codepoint off one `Uni` and
pushing it onto another, with an `nqp::elems` test per step. Inside a TRIR
body, each of those three ops reached the `Uni` behind two out-of-line calls,
and their call overhead cost more than the edit itself.

The bare-`Uni` path of `shift_i`, `push_i` and `elems` is now inlined into the
op loop. Lists, containers and anything else keep their path, now out of
line.

The 100-record SPDX decode (JSON::Fast 0.21.0) dropped by about 5% (callgrind).
`t/vm/codegen/adr8673-trir-uni-ops.t` now also runs the three ops on a native
int list, the out-of-line path, against TRIR off and against rakudo.
