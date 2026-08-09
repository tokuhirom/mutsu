# Role BUILD/TWEAK submethod on a `does`/`but`-mixed plain value drops array/hash attribute writes

`run_role_submethod` (`src/runtime/types/roles.rs`, the helper `call_role_build_submethods` uses
after `$value does Role` / `$value but Role` composes a role onto a non-`Instance` value such as an
`Int` or `Str`) seeds the submethod's attribute env vars using only the scalar twigil form:

```rust
let key = format!("__mutsu_attr__{}", attr_name);
if let Some(val) = mixins.get(&key) {
    self.env.insert(format!("!{}", attr_name), val.clone());
}
```

and reads them back the same way (`format!("!{}", attr_name)`). For a scalar attribute (`has $.x`)
this key (`"!x"`) matches what the compiled/interpreted body resolves `$!x` to. But for an array or
hash attribute (`has @.a` / `has %.h`), the body's `@!a` / `%!h` reads and writes resolve through the
sigil-prefixed env key (`"@!a"` / `"%!h"`), which is never seeded — so any `@!a.push(...)` or
`%!h<k> = v` inside a role's `BUILD`/`TWEAK` submethod silently no-ops when the role is composed onto
a plain, non-`Instance` value via `does`/`but`.

## Repro

```raku
role RH {
    has %.h;
    submethod BUILD { %!h<a> = 1 }
}
my $v = 0;
$v does RH;
say $v.h.raku;   # raku: "{:a(1)}", mutsu: "{}".Seq (empty)

role RA {
    has @.a;
    submethod BUILD { @!a.push(1); @!a.push(2) }
}
my $w = "x";
$w does RA;
say $w.a.raku;   # raku: "[1, 2]", mutsu: "[]"
```

Verified against Rakudo v2026.06 (raku binary on this machine): both attributes populate correctly.
Confirmed present on `main` at commit `18b6f7745` (pre-dates ADR-0019 D8-3's compiled-body cutover of
`run_role_submethod`; not a regression from that change — reproduces identically before and after).

## Fix sketch

`run_role_submethod`'s seed/readback loop needs to seed (and read back) the array/hash-shaped keys
too — likely `format!("@!{}", attr_name)` / `format!("%!{}", attr_name)` alongside the scalar
`"!{attr_name}"` form, picked by the attribute's declared sigil (`RoleDef`'s attribute list already
carries the sigil — see how `apply_role_mixin`'s default-value construction switches on `attr.sigil`
a few dozen lines above `run_role_submethod` in the same file). The mixin map itself already stores
the value under the plain `__mutsu_attr__{name}` key regardless of sigil, so only the env seed/readback
key needs to vary by sigil, mirroring how ordinary instance-attribute env keys are chosen elsewhere
(`ATTR_ALIAS_META_PREFIX` / `attr_twigil_local` helpers in `src/vm/vm_method_dispatch.rs`).

## Why not fixed inline with D8-3

D8-3 (ADR-0019, "run_role_submethod rider") is scoped to swapping the body's execution mechanism
(tree-walk `eval_block_value` -> `run_compiled_block_raw` over the precompiled chunk) without changing
behavior. This bug is a pre-existing attribute-key mismatch orthogonal to that swap — confirmed
identical on `main` before the swap — so fixing it belongs in its own PR with its own repro-driven
`t/` test, not bundled into the bytecode-cutover slice.
