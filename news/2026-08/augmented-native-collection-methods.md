# Augmenting `sort`/`map`/`first` (and friends) on `Array`/`List` was silently ignored

`augment class Array { method sort { ... } }` is legal raku — `Array` never declares its
own `sort` (it comes from `List`), so there is no redeclaration error, unlike the
already-known `augment class Str { method uc { ... } }` case. But calling `.sort` on a
plain `@array` still ran mutsu's native fast path and ignored the override entirely:

```raku
use MONKEY-TYPING;
augment class Array { method sort { "USER-SORT-OVERRIDE" } }
my @a = (3, 1, 2);
say @a.sort;   # raku: USER-SORT-OVERRIDE, mutsu (before): (1 2 3)
```

The same gap applied to `.map`, `.first`, `.min`/`.max`/`.minmax`, `.subst`, and the
`.Set`/`.Bag`/`.Mix`/`.Map`/`.Hash`/`.Seq`/`.IO`/`.encode`/`.decode` coercions — mutsu's
"lever A" native dispatch for these methods never checked whether the receiver's *native*
type (`Array`, `List`, `Hash`, `Str`, ...) had a user-declared override, because that check
only ever ran for `Instance`/`Package` receivers. A plain `Array` value carries neither, so
it fell through every check silently. The gap existed on **three independent, unguarded
tiers**: the arity-keyed Tier-1 native dispatch, the block-taking "lever A" fallback, and
the interpreter's own final by-name dispatch — all three needed the same fix.

Added one shared predicate, `Interpreter::native_lever_a_user_override`, consulted at all
three tiers plus the top of both `call_method_with_values`/`call_method_mut_with_values`:
a non-`Instance`/`Package` receiver now checks `has_user_method` against its native type
name before taking any native path, and dispatches through `run_instance_method` — threading
the receiver value itself as `self`, not a synthesized type object — when an override exists.

## A second, deeper gap: `class_mro` didn't know `Array`'s built-in ancestors

Making that predicate correct exposed a second, pre-existing bug: `has_user_method("Array",
"first")` answered `false` even when `List` (not `Array`) was augmented with `first` —
despite `@a.first` legitimately dispatching through `List` in Raku's MRO either way, and
`Array.^mro` (a *different* code path) correctly reporting `(Array) (List) (Cool) (Any)
(Mu)`. The cause: `class_mro_readonly`'s "not a registered class" branch only consulted a
small hardcoded table (`Match`/`Capture`/`IO::Spec`/...) for a *bare* type name, and the
richer `builtin_type_catalog` (which already carries every builtin leaf type's full ancestor
chain) only for a *bracketed* parametrized name like `Array[Int]`. A bare `"Array"` matched
neither and fell to `compute_class_mro`, which has no `ClassDef` to read parents from and
returned the class name alone — silently dropping `List`/`Cool`/`Any`/`Mu`.

Fixed by teaching both `class_mro_readonly` and `compute_class_mro` to consult the catalog
for a bare name too — as well as for the immediate parent of a *directly*-augmented builtin
type with no explicit `is` clause (`augment class Array {...}` previously defaulted to `Any`,
also dropping `List`/`Cool`).

The first version of that fix regressed `t/digest-battery.t`'s SHA3 implementation: it
matched a *bracketed* name like `Blob[uint8]` against the bare-name catalog lookup too,
short-circuiting the existing bracketed-handling branch that correctly includes `Blob` (the
base class) in the chain — the catalog's own `mro` field for a parametrized row tracks its
base via `roles`, not `mro`, so the naive lookup silently dropped `Blob` from a `blob8`
receiver's MRO and broke placeholder-parameter (`@^b`) `Positional` signature binding for any
sub taking a sized buffer argument. Fixed by ordering the bare-name catalog check strictly
after the existing bracketed branch.

Pin: `t/augment-native-lever-a-methods.t` (raku-verified byte-identical output).
