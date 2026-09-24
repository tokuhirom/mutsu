# Attribute access resolves its owner, keys and `self` once per method frame

First slice of [ADR-0121](../../docs/adr/0121-instance-attributes-live-in-per-class-slots.md) D1
(#9291): a `$!x` / `$.x` read or write inside a method no longer re-derives, on every
access, what the method frame already knows.

## What was paid per access

- **The owner class.** `method_class_stack` held a `String` per frame, so every method
  call allocated a copy of its owner's name, and every private `$!x` access assembled
  `Owner\0x` and `Owner\0$x` in a scratch buffer and interned both (a string hash and a
  table probe each).
- **Whether the owner is a role.** Asked of the registry, under its lock, on every access
  -- twice on the role path.
- **`self`.** Resolved by name through `get_env_self`, which walks the unit-scope, package
  and module-lexical redirects before it reaches the env probe.
- **The declared type of the attribute being written.** `self_attr_type_constraint`
  copied the class's MRO into a `Vec<String>` up to three times per store.

## What changed

- The method-class stack holds a `MethodClassFrame`: the owner as a `Symbol`, plus its
  role-ness, which the compiled-method dispatch already computed and now hands over (and
  which is otherwise memoized on the frame at the first access that asks).
- The qualified private keys and the sigil-prefixed key are memoized per
  `(owner, bare, sigil)`.
- A method body's invocant is its first local slot; the attribute paths read `self` from
  there. A closure or nested block inside the method, and a slot that does not hold an
  object, still resolve it by name.
- The read, write and in-place `~=` append paths share one resolution routine
  (`with_self_attr`) instead of three copies of it.
- `mro_syms_readonly` hands back the registry's cached `Arc<[Symbol]>` MRO, and the
  attribute type-constraint lookups walk it.

## Measured

Callgrind, 1 vs 10,001 iterations of a method loop, profiling build, warm, against the
same loop without the access (`tmp/cg-rd.raku`-style, see ADR-0121 §6):

| per iteration | before | after |
| --- | ---: | ---: |
| `$s = $!x` above the empty loop | 4,166 Ir | 3,302 Ir (-21%) |
| `$!y = $i` above the empty loop | 10,727 Ir | 7,985 Ir (-26%) |

This does not meet #9291's goal (every row within 2x of rakudo) and was not expected to:
what is left per access is the attribute-map probes, the read guard, the attribute's
type-constraint lookup on a write, and the general `SetLocal` cascade -- the slot layout
and per-site caches of D2/D3.

Pinned by `t/oo/attribute/attr-access-resolution.t`.
