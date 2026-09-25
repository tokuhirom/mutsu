# A generated accessor reads its slot before method dispatch runs

Two more parts of ADR-0121 D3 landed (#9291): the generated-accessor lane, and a memo for the
declared type of an attribute written inside a method.

## `$obj.x`

`$obj.x` on a variable compiles to `CallMethodMut`. That opcode runs a long chain of probes
before `try_fast_accessor_read` recognizes the generated accessor and reads the attribute by
name. The probes cover proto bodies, exception delegates, lazy lists, `Failure`, NativeCall and
the plain-method lane. Each one declines on every call.

A new lane (`src/vm/vm_accessor_lane.rs`) remembers `(layout id, method name) -> slot` after the
full path has answered a call with the accessor's plain read. The next call on an instance of the
same layout reads the slot before any probe runs. This follows the rule the plain-method lane
(#8880) already uses: the cache sits in front of the probe chain, not inside it.

The lane only replays what the full path did:

- **Written from one place.** It is filled right after the full path's accessor read succeeds.
  At that point every probe ahead of it has just declined.
- **Registry changes.** Methods, wraps, accessor visibility and MRO are pinned by
  `Registry::method_generation`, and the lane is cleared with the other method caches.
- **Class shape.** The layout id pins it.
- **Per-instance state.** Every hit checks that the instance has no undeclared attribute, that
  the slot is present, and that the value is a scalar. An `@.x` / `%.x` read has to attach its
  declared container type.
- **Excluded classes.** The lane never installs for:
  - classes the program did not declare;
  - CStruct classes;
  - `IO::Handle` / `IO::Path` descendants;
  - a deprecated attribute, because reading it warns.

In callgrind, an accessor call in a mainline loop cost **6,640** instructions more than
`$s = $i`. It now costs about the same as `$s = $i`: the measured difference is -49, which is
within noise.

## `$!y = v`

Every scalar attribute store inside a method looked up the attribute's declared type:

- `self` by name;
- a sigil-collision scan over the MRO;
- the declaring class's type;
- the nested-class qualification of that type.

The answer depends only on the class, the attribute name and the registry. It is now memoized
per `(class, attribute)` and keyed on `registry_write_generation`, which is the same scheme as
`numeric_bridge_probe`. Every registry write goes through `registry_mut()`, which bumps that
generation, so a class declared later (even through `EVAL`) cannot see a stale answer. Debug
builds re-derive the answer on every hit and assert that it matches.

A write went from **7,475 to 5,833** instructions above an empty method loop. A read is
unchanged at 2,970.

## What is left

On a write, the remaining cost is mostly the `SetLocal` store:

- it mirrors the attribute local into the env (`set_env_with_main_alias_inner`, ~1,800);
- it resolves `self` by name for the type lookup.

The first is the dual-store debt, not attribute storage. Also left under ADR-0121:

- D3: constant-name `getattr` / `bindattr` site caches, and `Array` / `Hash` `$!descriptor`;
- D4: `nqp::attrinited`.

Pins: `t/oo/attribute/accessor-lane.t`, `t/oo/attribute/attr-type-constraint-memo.t`.
