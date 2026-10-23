# A `Slot` element store no longer runs the env-seeding scaffold

`@a[$i] = $v` into a local the running code declares (`StorePlace::Slot`,
ADR-14632) takes its container from the slot, so `seed_env_from_scalar_slot`,
which made the env answer what the slot held, now runs only for a `Named` store.

`with_own_container_seeded` stays: a module routine's own `my %seen` key store
still resolves by name (`t/modules/module-routine-element-store-shadowed-file-lexical.t`
fails without it), so it serves a `Named` shape. `with_package_lexical_seeded` and
`restore_nested_store_local` stay too, documented as serving the `Named` shapes
(a package-block lexical, the nested-store ops). No behavior changes.
