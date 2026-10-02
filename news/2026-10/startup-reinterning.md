# Start-up stops re-interning names it already has

A bare script made 4,259 `Symbol::intern` calls before running a line of user code, for only 976
distinct names: 3,283 of the calls re-interned a name that was already a symbol (#10961, the
follow-up to the per-call slice). Interning was 23% of the start-up instruction count.

- `registry_has_destroy_methods`, asked once at exit, probed `(class, "DESTROY")` for every one
  of the ~450 registered classes, interning both names each time (892 calls). It now scans the
  method table's rows for a user `DESTROY` and checks that the owner is a class.
- `seed_builtin_method_entries` interned the owner type once per built-in method entry (701
  calls for 18 owners). It now interns each owner once.
- The registry build ran `sync_accessor_entries` for every class, interning each name for what
  is a no-op on a class without attributes, which covers most of the built-in `X::` tree. Only
  classes with attributes are synced now.
- The thread-local intern memo keyed its entries by an owned `String` copy of each name. That
  cost one allocation per distinct name, and one free per name at thread exit. It now keys on
  the global table's own leaked `&'static str`, which helps every new name, not only at start-up.

callgrind, `--profile profiling`, second run, against `main` @ `112766ef`:

| script | Ir |
| --- | ---: |
| `f()` loop, 0 iterations | 12,813,302 → 11,522,099 (−10.1%) |
| `$o.m()` loop, 0 iterations | 13,788,454 → 12,451,765 (−9.7%) |

Intern calls for the first script, counted with a `rust-gdb` breakpoint: 4,259 → 2,253.

One idea did not hold up. Building each `X::` exception's MRO from its parent's already-interned
MRO, instead of re-walking and re-interning the chain, found a reusable parent only 33 times out
of 368, because most parents are hand-built classes. The memo's own map cost more than it saved,
so it was dropped. What remains is close to the floor of one intern per distinct name. Going
lower would mean pre-building the table of built-in names, which is a design change.
