# A `Slot` element store no longer runs the env-seeding scaffolds

`@a[$i] = $v` into a local the running code declares (`StorePlace::Slot`,
ADR-14632) takes its container from the slot, so two scaffolds that made the env
answer what the slot held are gone for it:

- `with_own_container_seeded` is deleted (a routine's own `my %seen` is a `Slot`
  store now);
- `seed_env_from_scalar_slot` runs only for a `Named` store.

`with_package_lexical_seeded` and `restore_nested_store_local` stay, now
documented as serving the `Named` shapes (a package-block lexical, the nested-store
ops). No behavior changes; `t/collections`, `t/vm`, `t/types` and `t/lang` pass.
