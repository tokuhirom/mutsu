# Wrapped routines reached through an exported short package name; `Foo:D.REPR`

FontConfig's `t/00raw.t` now produces the same TAP under mutsu as under
rakudo. Two general gaps were in the way:

- `unit module FontConfig::Raw` exports nested packages (`class FcObjectType
  is export`, `module FcName is export`) whose `our sub ... is native {...}`
  routines are called by the short name: `FcObjectType::get-object-type(...)`.
  The NativeCall trait rebinds the routine's `$!do`, which mutsu models as the
  routine's wrap chain, and the qualified-call lookup of a wrapped routine only
  matched the routine's full spelling (`FontConfig::Raw::FcObjectType::...`),
  so the short spelling ran the stub body ("Stub code executed"). The lookup
  now resolves the call's package through the exported alias first, so any
  `.wrap` or `$!do` rebind is honoured on that path too.
- A definite (`Foo:D`, `Foo:U`) or coercion (`Int(Str)`, `Int()`) type object
  reported its base type's REPR. In rakudo these are made by `DefiniteHOW` /
  `CoercionHOW` and are `Uninstantiable`; NativeCall's `check_routine_sanity`
  relies on that, and warned about every `--> ::?CLASS:D` return of a
  `CStruct` class (`FcFontSet::create`).

A separate gap seen while reducing it, a trait's `CALL-ME` mixin being ignored
by a package-qualified call, is #13374.
