# A user-defined tied container's `STORE` survives the capture cell

Dropping the by-name `is <Type>` capture-cell exclusion (`fdb5bd18`, PR #7520)
put an `is <Type>` declaration's value inside a shared `ContainerRef` cell. That
commit converted the three consumers it knew about — `exec_apply_var_trait_op`
through `read_var_trait_target`/`write_var_trait_target`, the declaration
store's metadata tagging, and `coerce_hash_var_value` — which between them cover
the built-in `is SetHash` / `is BagHash` / `is MixHash` coercions it was
measured against.

The **tie dispatch** was a fourth consumer, and it is the one only a
*user-defined* container class reaches. `tied_instance_type_name` matched
`Instance` and `Mixin` but not `ContainerRef`, so from that commit onward a
user-defined tied container was not recognized as tied at all:

```raku
role OddThrower does Associative {
    method STORE(*@v) { say "USER STORE" }
    method AT-KEY($) { Nil }
    method keys()    { () }
}
class OT does OddThrower {}
my %h is OT;
%h = "a";        # rakudo: USER STORE
                 # mutsu:  Odd number of elements found where hash initializer expected
```

Every shape was affected — top level, inside a bare block, captured across a
call boundary, and declared inside the invoked block — not only the captured
one.

## Why nothing caught it

`t/tied-hash-store-oddnumber.t` exists precisely to pin this, and it kept
passing, for two independent reasons:

- Its `throws-like` matchers on `.found` / `.last` were **skipped** by mutsu's
  native `Test` provider, which hard-codes `# SKIPPED matcher '.found': mutsu's
  X::Hash::Store::OddNumber carries no such attribute`. The exception that
  actually arrived was mutsu's own built-in `X::Hash::Store::OddNumber`, which
  has no such accessors — exactly the case the skip was written for, so it
  silently absorbed the regression.
- Its top-level assertion catches with `CATCH { when X::Hash::Store::OddNumber
  { ... } }`, and mutsu's built-in exception matches that by name just as well
  as the user's.

The vendored upstream `Test` module has no such skip, so the switch to it
(`news/2026-09/vendored-test-module-is-the-default-provider.md`) turned the
regression into a hard failure — `No such method 'last' for invocant of type
'X::Hash::Store::OddNumber'`, planned 8 / ran 4 — within hours of `fdb5bd18`
landing. That is the retirement of a native provider paying for itself: a
hard-coded compatibility skip is exactly the kind of blind spot BATTERIES.md §1
warns a private dialect creates.

## The fix

`tied_instance_type_name` looks through a `ContainerRef`, and the two
reassignment entry points (`maybe_tied_store_reassign` and its `env`-named twin)
dispatch on the dereferenced instance and publish the bound result **through**
the cell rather than over it, via a new `publish_tied_bound` that carries the
same contract `write_var_trait_target` documents: when the write went through a
cell, the caller must not also `set_env_with_main_alias`, or env ends up
de-celled while the slot stays celled and the two halves disagree about the
variable's identity.

`fdb5bd18`'s own repro (a dead `my %h is BagHash` branch no longer de-vouching a
same-named `%h`) still answers 4, and `is BagHash` still takes its initialiser.

Pinned by `t/tied-container-store-through-capture-cell.t` (9 subtests, identical
output under `raku`, and asserting on the STORE actually running rather than on
the exception type — so neither hiding mechanism can absorb it again).
