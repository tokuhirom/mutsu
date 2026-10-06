# ADR-11203: NativeCall runs upstream verbatim through its backend-neutral path

- **Status**: Accepted (user decision 2026-10-03). Implemented (the switch landed); see §5.
- **Date**: 2026-10-03
- **Deciders**: tokuhirom, Claude
- **Issue**: [#11203](https://github.com/tokuhirom/mutsu/issues/11203)
- **Supersedes**: [ADR-0096](0096-batteries-adoption-policy.md) §D4/E1 (`NativeCall` as a
  justified rung-3 exception). The rest of ADR-0096 stands.
- **Related**: [#7560](https://github.com/tokuhirom/mutsu/issues/7560) (the original
  measurement and its 2026-10-03 re-measurement),
  [ADR-0015](0015-native-backed-container-storage-and-repr-bodies.md) (native-backed storage and
  REPR bodies, which this builds on)

## 1. Context

ADR-0096 lets a module stay a native provider (BATTERIES.md rung 3) only while a written record
states what makes rung 2 unreachable. `NativeCall` was the last entry on that list (E1). Its
record, #7560, rested on two structural blockers:

1. `use QAST:from<NQP>` — "NativeCall builds each `is native` sub's body as a QAST tree".
2. `NativeCall::Dispatcher` is written against MoarVM's dispatch programs
   (`nqp::syscall`/`track`/`guard`/`delegate`/`register`), and exposing an equivalent surface is
   a VM design change.

Re-reading the upstream source on 2026-10-03 (rakudo 2026.06, the release
`modules/Rakudo-Core/` vendors, and 2026.09) showed that neither blocker is real:

1. **QAST is a dead import.** `use QAST:from<NQP>;` on line 2 of `NativeCall.rakumod` is the
   only occurrence of `QAST` in all five files (`NativeCall.rakumod`, `NativeCall/Types.rakumod`,
   `NativeCall/Dispatcher.rakumod`, `NativeCall/Compiler/{GNU,MSVC}.rakumod`). mutsu already
   compiles `use X:from<NQP>` as a no-op.
2. **The dispatcher is optional upstream.**

   ```raku
   my constant $use-dispatcher :=
       $*RAKU.compiler.?supports-op('dispatch_v')
         ?? do { require NativeCall::Dispatcher; True }
         !! False;
   ```

   On a backend without `dispatch_v` (the JVM backend, and mutsu, where the expression is
   already `Nil`), `setup-nativecall` takes a backend-neutral path instead. It builds
   `-> |c { ... nqp::nativecall($!rettype, self, $args) ... }` and binds that closure's `$!do`
   into the routine. `NativeCall::Dispatcher` is never loaded.

So upstream NativeCall runs on mutsu **without a single edit** once the interpreter supplies
what that path uses. ADR-0096 rejects patching vendored sources, so it matters that no edit is
needed. What is needed is ordinary interpreter growth. Excluding the control forms
(`if`/`while`/`stmts`), the non-dispatcher path uses 50 distinct `nqp::` ops, and 14 of them are
missing. A trial load of the five files under a renamed namespace (the real names are
intercepted today) stopped only on general Raku gaps, listed in §4.

## 2. Decision

1. **`NativeCall` moves to rung 2.** mutsu vendors `lib/NativeCall.rakumod` and
   `lib/NativeCall/**` from the same rakudo release as the rest of `modules/Rakudo-Core/`,
   verbatim. `use NativeCall` and `use NativeCall::Types` resolve to those files. The
   name-keyed interception and the native module provider are deleted.
2. **mutsu takes the backend-neutral path, on purpose.** `$*RAKU.compiler.?supports-op('dispatch_v')`
   stays falsy. mutsu does not grow a dispatch-program API (`nqp::syscall`/`track`/`guard`/
   `delegate`/`register`) for NativeCall's sake. `NativeCall/Dispatcher.rakumod` is vendored
   with the rest so that the tree stays upstream's, but nothing loads it.
3. **The FFI itself stays in the VM, as six `nqp::` ops.** `nqp::buildnativecall`,
   `nativecall`, `nativecallcast`, `nativecallsizeof`, `nativecallglobal` and `nativecallrefresh`
   are MoarVM's FFI layer, written in C inside MoarVM. In mutsu they are Rust inside the VM,
   built from the existing `src/runtime/nativecall*.rs` machinery (libloading + libffi, callbacks,
   casts, globals). That machinery must take its marshalling from the argument- and
   return-info hashes that upstream's `param_hash_for`/`return_hash_for` build, not from its
   own reading of the routine's signature. This is the same split MoarVM has. It is not a rung-3
   provider, because every Raku-level line of `NativeCall` is upstream's.
4. **The REPRs are selected by `is repr<...>`, not by class name.** `CArray`, `CStruct`, `CUnion`,
   `CPPStruct`, `CStr` and the `NativeCall` callsite REPR attach their storage to whatever class
   declares them, as ADR-0015 already does for the bodies it covers.
5. **The switch is made only when nothing regresses.** 35 bundled files `use NativeCall`
   (OpenSSL, DBIish, IO::Socket::SSL, Crypt::Random, ...), so the native provider stays the
   default until the vendored module passes every bundled-library suite and every whitelisted
   roast file that touches NativeCall. This repeats the precedent of `Test` (#7566), which was
   switched only after the `Bundled-library test suites` gate stopped regressing.

## 3. Options considered

- **Keep E1 (status quo).** Rejected: its stated rationale has expired, and ADR-0096 §D4 says
  an expired rationale is re-decided rather than re-cited.
- **Vendor with a small patch** (drop the QAST import, hard-wire `$use-dispatcher` to `False`).
  Rejected, and unnecessary: ADR-0096 rejects patched trees because every upstream bump becomes
  a merge, and both lines already behave correctly on mutsu unmodified.
- **Implement the dispatch-program API and take the MoarVM path.** Rejected for now. It is a
  core VM design surface that nothing else needs, and the backend-neutral path is a supported
  upstream code path. If a future upstream drops that path, this decision is revisited with a
  new ADR.
- **Implement QAST.** Not needed; see §1.

## 4. Work, as issues

Each is a general compatibility gap; none is NativeCall-specific.

| Issue | Gap |
| --- | --- |
| [#11204](https://github.com/tokuhirom/mutsu/issues/11204) | Native int/num types: `P6int`/`P6num`, `int32 ~~ Int`, `is unsigned`, `.^unsigned` |
| [#11205](https://github.com/tokuhirom/mutsu/issues/11205) | An anonymous `multi` routine as a term, and its `.dispatcher` |
| [#11206](https://github.com/tokuhirom/mutsu/issues/11206) | `nqp::unbox_n`/`unbox_u`/`bindpos_u`/`atposref_{i,n,u}`/`neverrepossess`/`setcodename` |
| [#11207](https://github.com/tokuhirom/mutsu/issues/11207) | `Code.$!do` as a readable and rebindable attribute |
| [#11208](https://github.com/tokuhirom/mutsu/issues/11208) | `Str.naive-word-wrapper` |
| [#11209](https://github.com/tokuhirom/mutsu/issues/11209) | REPRs selected by `is repr<...>`: `CArray`, `CStruct.new`, `CUnion`, `CPPStruct`, `CStr`, `NativeCall` + `is box_target` |
| [#11211](https://github.com/tokuhirom/mutsu/issues/11211) | The six VM FFI ops |
| [#11203](https://github.com/tokuhirom/mutsu/issues/11203) | Integration: vendor, remove the interception, pass the gates, delete the provider |

## 5. Implementation status

| Step | State |
| --- | --- |
| Decision recorded; ADR-0096 E1 marked superseded | Done (#11225) |
| #11206 non-FFI `nqp::` ops | Done (#11239) |
| #11208 `Str.naive-word-wrapper` | Done (#11248) |
| #11204 native type semantics | Done (#11260) |
| #11205 `multi` term evaluates to its candidate | Done (#11278) |
| Upstream files vendored (`modules/Rakudo-Core/lib/NativeCall*`), not yet in `provides` | Done |
| #11310 user `trait_mod:<is>` candidates leak across compunits; `is array_type` is core | Done; `load UNC` now stops at `nqp::nativecallsizeof` (#11211) |
| #11209 REPRs selected by `is repr<...>` | In progress: `nqp::create` keeps a mixin type's roles; `is repr('CArray')` gives native element storage typed by `.^array_type` (a mixed-in role's trait included); `nqp::atposref_{i,u,n}` on native storage answer `IntPosRef`/`UIntPosRef`/`NumPosRef`. The trial's `CArray[int32]` steps pass. Reference-element `CArray` (`Str`, `Pointer`, CStruct; ADR-0015 P3c) has an address table plus a child table (`src/runtime/carray_ref.rs`); `.new`/`bless` allocate it; a `C[T]` constraint for a class with its own `^parameterize` is checked against the built type. The `NativeCall` REPR and `is box_target` are done (`src/runtime/box_target.rs`): `is repr<NativeCall>` is reported by the class and its instances; `is box_target` is a core attribute trait recorded per declaring class or role; a class-typed box target is allocated with its type's REPR at construction (the parser seeds `Type.CREATE`); `nqp::buildnativecall` and `nqp::nativecall` on an object with a box target apply to that attribute's value. `nqp::box_{i,n,s,u}` into a class with a box target store the value there and `nqp::unbox_*` read it back (`src/runtime/box_native.rs`, the one rule behind all eight ops). The `CStr` REPR is done (`src/runtime/cstr_repr.rs`): `is repr<CStr>` classes are registered by declaration; `nqp::box_s` into one makes an object that owns a leaked NUL-terminated UTF-8 copy (whatever `.encoding` says, as in Rakudo), `nqp::unbox_s` decodes it (a NULL one is the null str), and a `char*` parameter handed such an object, or a `Str` that did `ExplicitlyManagedString` (its `cstr` holds one), gets that buffer. The provider's own class-name-keyed `NativeCall::CStr` stays until the provider is deleted. The vendored 2026.06 `explicitly-manage` returns the `CStr` object where 2026.09's returns the `Str`; the trial step accepts both. `CStruct.new`, `CUnion` and `CPPStruct` objects built in Raku own a native body ([ADR-11209](11209-cstruct-new-allocates-native-storage.md)): zeroed, aligned, freed with the object, passed to C as a real pointer, their reference fields kept alive. A `CArray` inside a `CStruct` is selected by `is repr('CArray')` too (a field of such a class, lexical or not, is one pointer and reads back as that class over the memory it points at; a `HAS` inline array and a `CArray[T]` field read back as upstream's mixin type; `nativecast` to the bare class boxes by REPR), and the reference-element view over C memory (`nativecast(CArray[Str], …)` / `CArray[Pointer]`) works with the upstream types. Measured with every `t/` file that mentions NativeCall rewritten onto the vendored types (the trial's rename): the REPR tests pass; what is left there is the provider's own surface (names, preludes, `explicitly-manage` as a routine), tests that run subprocesses or relative paths, and [#12121](https://github.com/tokuhirom/mutsu/issues/12121) (a `CArray`-alias parameter does not bind a mixin instance). Open: nothing in the REPR list itself; the switch PR (#11203) owns the rest |
| #11211 the six VM FFI ops | Done (`src/runtime/nativecall_nqp.rs`, `nativecall_info.rs`); `load UNC` and `nativesizeof` now pass. A routine's callsite is built once, in its `is box_target` attribute (#11209) |
| #11207 `Code.$!do` readable and rebindable | Done: a bound body is the innermost entry of the routine's `.wrap` chain (`src/runtime/code_do_attr.rs`) |
| Vendored module is what `use NativeCall` loads | Done (#11203). `modules/Rakudo-Core/META6.json` provides `NativeCall`, `NativeCall::Types`, `NativeCall::Compiler::{GNU,MSVC}`; the name interception in `use`, the parser's module scan and the begin-use preload is gone. Upstream's `is native` trait applies through its own `Native` role (`$r does Native[...]`, `nqp::buildnativecall`/`nqp::nativecall` over `is box_target`). Fixed on the way: `size_t`/`long`/... are `native` declarations again once ADR-0056's bare-key display qualification is gone; `CArray:D`/`Pointer:U` resolve a constant alias; a signature resolves a parametric alias (`CArray[int32]`, `Pointer[void]`); a hoisted routine that re-arrives with an alias return spelling is the installed declaration (an `is native` sub in a parametric role body); a `Pointer` field declared through the alias reads back as upstream's type; an imported dispatcher whose family has no matching candidate no longer re-enters itself (it overflowed the stack on an unknown trait with NativeCall loaded) and a captured multi with no applicable candidate is `X::Multi::NoMatch` |
| Native provider deleted | Done (#11203): the `GLOBAL::Pointer`/`void`/`OpaquePointer`/`NativeCall::CStr` prelude, the six prelude subs and `trait_mod:<is>` candidates, `register_nativecall_exports`, the `__mutsu_nativecast`/`__mutsu_nativesizeof`/`__mutsu_cglobal_fetch`/`__mutsu_explicitly_manage` helpers, `register_native_call_sub`/`_method` and every place that consumed `is native` by name (`native_call_specs`, the call opcodes, method dispatch), the function-pointer module, `explicitly_managed_address`, and the `CArray` row in the core type catalog. Left for a follow-up: provider-era fallbacks inside the marshaller (`make_native_handle`, `make_typed_pointer`, the name-keyed `CArray` branches of `value_carray`/`methods_aggregate_ctor`), which upstream's types never reach |

The pre-switch measurement tool `scripts/nativecall-upstream-trial.sh` (the vendored
files under a renamed namespace) was deleted with the switch: the real names are the test now.

## 6. Consequences

- ADR-0096's exception list becomes empty once §5 completes. Until then E1 is "superseded,
  being executed", not "justified".
- The native provider's divergences from upstream (whatever its tests do not cover) go away
  with it, which is ADR-0096's main argument for rung 2.
- mutsu commits to keeping the backend-neutral path working. A NativeCall regression from now
  on is an interpreter bug, found by the vendored module's own behaviour and by the batteries'
  suites.
