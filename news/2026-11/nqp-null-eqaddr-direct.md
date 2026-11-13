# `nqp::null` and `nqp::eqaddr` skip the op-table walk

`nqp::while(COND, nqp::null)` and `nqp::eqaddr($cmp, Order::Less)` run once
per step of Array::Sorted::Util's binary search and equal-key scans. Both
went through the general nqp op path: an argument vector, the Proxy
normalization and a walk into the Text / Process op tables. With operands that
need no normalization (no `VarRef`, no `Proxy`) they now answer directly, with
the same one-shot callsite-line clear the pure ops keep. Behaviour is
unchanged; a divergence from rakudo in `nqp::eqaddr` on Scalar containers that
predates this was filed as #17620.
