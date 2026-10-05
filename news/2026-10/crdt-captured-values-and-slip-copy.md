# CRDT closures and aggregate copies preserve their values

The remaining CRDT failures depended on lexical capture, aggregate accessor context and Slip
assignment behaving consistently across the compiler and runtime.

- A fresh scalar declaration that shares an array no longer reuses stale captured state. A local
  shadow therefore cannot redirect an escaped closure to the outer scalar.
- Reading an array-valued accessor with `@.attribute` uses List context. Assigning a Slip to a scalar
  lvalue preserves its item semantics, and converting an empty Slip to a BagHash produces an empty
  BagHash.
- Default sorting resolves proxy values through their dispatched `cmp` and compares the fetched
  values, so ordering does not depend on proxy identity.

CRDT 0.0.16 now passes all 10 baseline test files under mutsu with Rakudo parity.
