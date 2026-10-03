# Switching `use NativeCall` to the vendored module, measured

The trial script for upstream NativeCall passes every step. A closer look shows that this does not
mean upstream's `is native` path works. mutsu still consumes the `is native` trait natively, even
under the trial's renamed module, so those steps run the native provider's call path with
upstream's types.

The unmerged branch `exp/11203-nativecall-interception-off` does the real switch:

- the vendored files go into `provides`;
- the `use NativeCall` no-op and the three NativeCall preludes go away;
- `is native` is no longer consumed natively.

On that branch, upstream's trait runs and the first native call stops because
`Native!setup` reads its `INIT my Lock $setup-lock` as Nil (#11528). The measurement also found:

- a `&trait_mod:<is>` returned from `sub EXPORT` is not installed when the importer has no other
  candidates (#11530);
- a SIGSEGV when the native provider is handed a pointer argument it does not recognize (#11529);
- the REPR gaps left in #11209: CPointer boxing, reference-element `CArray`, `CStr`, `HAS`.

Running every `t/` file that uses NativeCall against the renamed upstream module, with the native
call path still in place, 65 of 94 pass. The ADR-11203 status table and the trial script now carry
the caveat.
