# An untyped `CArray.new` is a `CArray`

`use NativeCall; my $a = CArray.new` used to answer a plain `Array` from the native
provider. `$a ~~ CArray` was False, `$a.^name` was `Array`, and `sub f(CArray $x)` refused it
with `Type check failed in binding to parameter '$x'; expected CArray but got Array ([])`,
while `CArray[int32].new(1, 2)` was already a real typed `CArray`.

The untyped spelling now carries `CArray` as its declared type, the way the
reference-element spellings (`CArray[Str]`, `CArray[Pointer]`) already did, so it
smartmatches `CArray` / `CArray:D`, binds a `CArray` parameter and reports
`NativeCall::Types::CArray`. Three related answers moved with it, all matching Rakudo:

- `.^name` of an `Array`-backed `CArray[T]` instance is package-qualified
  (`NativeCall::Types::CArray[Str]`; it printed `CArray[Str]` before), because the
  declared-type branch of `.^name` now goes through `user_facing_type_name` like
  the instance and type-object branches next to it;
- an untyped, empty `CArray` handed to a native routine's `CArray` parameter is a NULL
  pointer, as MoarVM's empty storage is, instead of failing with `CArray parameter is
  missing its element type`;
- `scripts/nativecall-upstream-trial.sh` has a step for the vendored upstream class
  (`UNC::Types::CArray.new` is a `CArray`, binds a `CArray` parameter), which already
  answered correctly through `is repr('CArray')` ([#11209](https://github.com/tokuhirom/mutsu/issues/11209)),
  so the switch in [#11203](https://github.com/tokuhirom/mutsu/issues/11203) cannot
  regress it unnoticed.

The provider-side change is deliberately the small one: the untyped spelling joins the
form the other reference-element spellings use, and it goes away with the provider
([ADR-11203](../../docs/adr/11203-nativecall-runs-upstream-via-the-backend-neutral-path.md) §5).
Not changed, and still different from Rakudo: an untyped `CArray` is still `Array`-backed, so it
accepts `$a[0] = 1`, `.push` and `~~ Positional`, where Rakudo dies with "CArray cannot be used
without a type"; `.REPR` stays `P6opaque`.

Closes [#12085](https://github.com/tokuhirom/mutsu/issues/12085).
