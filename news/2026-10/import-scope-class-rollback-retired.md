# A block's `use` no longer rolls back the class table

When a block that imports something closed, its import scope dropped every
class registered since the block opened, except those on a growing list of
exceptions:
- a module's qualified classes;
- the classes a module's body declared;
- types minted at run time;
- `our` classes the block declared;
- the parents of all of the above.

Each exception had been added after a regression (#9532 and others). The
rollback existed to make an imported type name leave with its block.

Since ADR-8751's type slices, an imported type is a term of the block's pad,
and its type positions are qualified at parse time. Nothing the rollback still
dropped was an import any more. With it disabled, `t/modules` and the
whitelisted `roast/S11-modules` files stay green. So the class half of
`ImportScope` is gone, together with `persistent_classes` (written only for it)
and the O(C) term of the scope's exit.

The `our`-class exception (c23a5b6e68) is reverted along with it: a class a
block declares now outlives the block simply because nothing drops it.
