# `Signature.arity`/`.count` are now correct for raw-capture (`|c`) params

`Signature.arity`/`.count` (and the equivalent `Code`/`Sub` methods) computed
positional arity/count by filtering on `!p.slurpy`, but a raw-capture
parameter (`|c`) sets `SigParam::slurpy` to `false` (it is a distinct kind
from a regular slurpy, tracked via `is_capture`), so it fell through both
filters and was counted as one required, bounded positional parameter:

```
$ raku -e 'sub foo(|c) {}; say &foo.signature.arity; say &foo.signature.count;'
0
Inf
$ mutsu (before this fix) -e 'sub foo(|c) {}; ...'
1
1
```

Real Rakudo: a raw capture contributes 0 to `.arity` (it captures whatever
remains, not a required positional) and makes `.count` unbounded (`Inf`),
since the signature accepts any number of further arguments.

## Fix

`signature_required_positional_count` and `signature_positional_count`
(`src/runtime/methods_signature_candidates.rs`) now also exclude/short-circuit
on `p.is_capture`, matching how they already treat `p.slurpy`. Both the
direct `&sub.arity`/`.count` path and the `.signature.arity`/`.count` path
share these functions, so the fix covers both.

Regression tests added to `t/signature-arity-count.t`.
