# The rest of the built-in registry stops being built at start-up

`build_builtin_registry` had three pieces of work left after the class
catalogue moved to first use (ADR-19206):

- **Empty tables.** A `Registry` has about forty sharded tables, and every
  empty one allocated its shard array and an empty table. A `ShardedMap` now
  creates its shards on its first write, so an empty table allocates nothing,
  and cloning one is a copy of `None`.
- **The composed roles of the built-in value types** (`Int does Real`,
  `Array does Positional`, the `Systemic` classes, ...). They were written as
  `Vec`s of `String`s into two registries at every start. They are now static
  rows (`builtin_composed_role_seeds`) in the same layout as the class
  catalogue, which `ComposedRoleTable` answers from on first lookup, as it
  already did for the catalogue's `X::` rows.
- **The built-in roles.** Their 28 definitions were built into a staging map
  and then converted into the registry's sharded table, each one a full
  `RoleDef` literal. They are now two lists of names inserted straight into
  the table (`runtime_init_builtin_roles`).

Refs #19197.
