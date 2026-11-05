# The compound-assignment subscript temporary is a plain slot-only local

`%h{$i % 50} += 1` still needs a temporary to evaluate the subscript once (an operator
may be overloaded, so it cannot be evaluated twice). That temporary, `__mutsu_idx_N`, was
excluded from the plain-lexical names because of its `__mutsu_` prefix, so its declaration
missed the slot-only declaration lane and ran the full `SetLocalDecl` cascade (about 1,280
instructions plus name-keyed env work). It is only ever read through its own slot, so the
prefix check now exempts `__mutsu_idx_`.

Measured (profiling build, callgrind second run, slope 1,000 -> 3,000 iterations of
`%h{$i % 50} += 1`): 12,880 -> 8,901 Ir per iteration; `%h{$k} += 1` (no temporary) is 5,756.
Pinned by `t/collections/subscript/compound-assign-operator-subscript-once.t`.
