# EXPORT stash pairs, `$?CLASS` in role bodies, readonly marks leaking into fresh bindings

Three fixes found while working on SBOM::CycloneDX:

- **`sub EXPORT` returning `UNIT::{$_}:p` pairs.** Each value arrived in its
  stash container. A `&name` key was already unwrapped, but a sigilless type
  was installed still wrapped, so `LicenseId("0BSD")` (a `my class` exported
  this way, called through its `CALL-ME`) died with "Unknown function". Type
  and term keys are now unwrapped too.
- **`$?CLASS` in a role body is the composing class.** The class body sets
  `?CLASS` only after its roles are composed, so a role body saw the
  *previously* registered class (`Nil` for the first). It is now bound to the
  class being composed for the duration of the body.
- **Readonly marks no longer leak into fresh bindings.** The readonly set is
  keyed by bare name. A `my @names is List` in a module, class or role body
  left its mark behind, and every later `@names` parameter or `my @names`
  elsewhere became silently unassignable. SBOM::enums'
  `sub EXPORT(*@names) { @names ||= ... }` then exported nothing. A
  declaration and an `@`/`%` parameter now clear an inherited mark for their
  own frame. The original binding stays immutable.

SBOM::CycloneDX itself still waits on #11516: role bodies run before the
composing class's own attributes are declared.
