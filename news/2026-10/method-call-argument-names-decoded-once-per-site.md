# A method call with an argument no longer re-decodes and re-allocates its argument names

`$o.m($x)` cost 1.40x a zero-argument `$o.m()` (callgrind, skeleton subtracted, 13,348 vs 18,693
instructions); it now costs 1.21x (13,321 vs 16,103). The call site's argument-source names are
decoded once per site and shared as one `Arc` with the callee instead of being rebuilt, cloned
and freed on each execution, the slot map is keyed by `Symbol` with a fast hasher, and the plain
lane reuses its key buffer (#10959).
