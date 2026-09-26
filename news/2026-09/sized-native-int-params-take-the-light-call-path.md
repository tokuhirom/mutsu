# Sized native-int parameters take the light call path

Before this change, a routine with a sized native-int parameter (`uint32 $n`, `int8 $x`, `byte $b`, and the C-width aliases such as `long`) never took the light call path ([#9506](https://github.com/tokuhirom/mutsu/issues/9506)). `FastParamType::of` classified only `Int`/`Str`/`Num`/`Bool`/`Rat`, the full-width natives `int`/`str`/`num`, and `Any`/`Mu`. Every call therefore went through `call_compiled_function_named`, the general binder, and a by-name type check. `Digest::RIPEMD`'s `sub rotl(uint32 $n, $b)` is called twice per compression step, so this path dominated its profile.

A new `FastParamType::NativeIntSized` tag covers every other spelling that `native_types::is_native_int_type` knows. The tag admits exactly what the `int` tag admits: an `Int`, `BigInt`, `Bool`, an Int allomorph, or an Int-valued enum. Both light bind sites then call the general binder's own `wrap_native_int_for_binding` under the parameter's spelling, so the light path binds the same value the general binder would, including its width wrap. The error path matches too. A parameter that fails the check gets the binder's `X::TypeCheck::Binding::Parameter` ("expected uint8 but got Str ("x")"), and a type object gets its "Cannot unbox a type object" error. That keeps the errors these routines already produced, even though the light path's generic type error has different wording.

Two places where mutsu's binder differs from rakudo (the width wrap on bind, and an enum argument that is not unboxed) are left unchanged here, so this perf change does not alter behaviour. They are tracked as [#9533](https://github.com/tokuhirom/mutsu/issues/9533).

Release build on a 4-core container, 200k calls (`tmp/sizedperf.raku` from the issue):

| | before | after |
| --- | ---: | ---: |
| `sub gu(uint32 $n, $b)` | 0.75-0.86 s | 0.11-0.13 s |
| `sub g($n, $b)` | 0.21 s | 0.23 s |

`t/nativecall/sized-native-int-light-call.t` checks that the light path matches the general binder: wrap, Bool, allomorph, enum, repeated calls, `~~ uint32` introspection inside the body, and the three error shapes.
