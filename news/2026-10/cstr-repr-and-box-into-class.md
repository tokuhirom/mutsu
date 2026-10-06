# The `CStr` REPR, and `nqp::box_*` into a class

`nqp::box_i`, `box_n`, `box_s` and `box_u` now box into a class the way
MoarVM's do, and `nqp::unbox_*` read the value back (#11209, ADR-11203 §2.4).
A class with an `is box_target` attribute (`has int $!value is box_target`)
gets an instance of itself with the native value in that attribute; the class
may declare it directly, inherit it, or compose it from a role. Any other type
operand (`Int`, `Str`, a plain class) still answers the plain value, so
existing callers are unchanged. The eight ops share one rule
(`src/runtime/box_native.rs`) instead of one answer each.

On top of it, a class declared `is repr<CStr>` is a real REPR now, selected by
the declaration and not by a class name. `nqp::box_s($str, CStr)` makes an
object that owns a NUL-terminated UTF-8 copy of the string which the runtime
never frees, `.REPR` reports `CStr`, and `nqp::unbox_s` decodes it again (an
object made with `nqp::create` and nothing boxed unboxes to the null str).
Whatever `method encoding()` the class declares, the bytes are UTF-8, which is
what Rakudo hands C (measured for `ascii` and `utf16`).

A `char*` parameter that receives such an object, or a `Str` that did
`ExplicitlyManagedString` and so carries one in its `cstr` attribute, is
given that buffer instead of a temporary copy. This is what upstream
NativeCall's `explicitly-manage` needs: `putenv(explicitly-manage('A=1'))`
keeps working after the call because the buffer lives on. The upstream
`explicitly-manage` now runs in `scripts/nativecall-upstream-trial.sh`.

The trial script also stopped renaming `repr<NativeCall>` to `repr<UNC>`:
that is the VM's name for a REPR, and the rename had silently turned the
upstream `Callsite` into an ordinary class.

`t/nativecall/nativecall-box-into-box-target-class.t` and
`nativecall-cstr-repr.t` pin both against Rakudo. Still open on #11209:
`CStruct.new` storage, `CUnion`, `CPPStruct`, `HAS` sizes and a reference-
element view over C memory.
