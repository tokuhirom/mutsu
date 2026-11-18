# A wrap-chain lookup no longer allocates for a class that was never wrapped

`Registry::method_wrap_chain` keys its chains by `(owner class, method name, candidate index)` as
owned `String`s. A lookup therefore built two `String`s, and hashed them, before it could miss.
Its only prefilter was the method name. So in a program that wraps one class's `pick` (as
Method::Protected's `is protected` does), every `.pick` call on any value checked the chains of
each class of the receiver's built-in MRO: `Array`, `List`, `Cool`, `Any`, `Mu` and its roles. That
cost two allocations per class, about 50 allocations per call.

A new conservative prefilter, `Registry::method_wrap_owners`, records each class that ever had a
chain entry. Like the method-name sets beside it, a stale member costs only one lookup, so entries
are never removed. Every creation of a chain entry records its class first. `copy_type_entries`
unions the set, and its exhaustive destructuring of `Registry` means a new field cannot be
forgotten there. A lookup for any other class now answers a miss without allocating.

The built-in probe, `builtin_method_wrap_chain`, also answers `None` up front when the receiver is
a built-in (catalog) type and every owner is a class the program declared under a non-catalog
name. A built-in type never inherits from a user class. Before this, the probe scanned the catalog
linearly for the receiver's MRO (`builtin_type_mro_strs`) and then built the receiver's ancestors as
a `Vec<String>`, on every call.

Callgrind slope (profiling build, N 300 -> 900, main vs this change):

| | before | after |
| --- | ---: | ---: |
| `@words.pick` after an unrelated class's `pick` was wrapped | 41.7k Ir | 16.9k Ir |
| Method::Protected `is protected` pick, `srand(42)` | 225.7k Ir | 191.1k Ir |

The same `@words.pick` costs 3.3k Ir when no `pick` is wrapped at all. The rest of the gap is the
general method-call path that a call takes once its method name was ever wrapped.

`t/oo/method/wrap-method-same-name-other-class.t` pins the lookups that must still find a chain:
the wrapped class, its subclass, a class wrapped later, and a built-in wrap found through the MRO.
It also pins that same-named methods elsewhere stay unwrapped. Refs #12875.
