# NativeCall: `Pointer[T]` parameter types, and `CArray.allocate`

`sub g(Pointer[uint16] $p) { ... }` failed at compile time with `Pointer cannot
be parameterized`, even though `my Pointer[uint16] $p .= new` worked fine.
`Pointer` is spliced into every program that uses `NativeCall` as a genuine
`class GLOBAL::Pointer` prelude, so the compile-time signature pre-pass that
validates parameter type constraints collected it into its `declared_classes`
set — the same set it uses to reject `UserClass[T]` as `X::NotParametric`. The
runtime type-parameterization check already special-cased `Pointer` as
parametric, but the compile-time pre-pass had its own, separate allowlist that
didn't. The two are now one shared list
(`runtime_class_query::is_parametric_builtin_type_name`), consulted by both.

`CArray[T].allocate(n)` was also entirely missing — `CArray.new` followed by
an out-of-range element assignment (`$arr[n - 1] = 0`, the workaround
`Language/nativecall.rakudoc` documents predates Rakudo 2018.05's `allocate`)
was the only way to pre-size a buffer. It now pre-sizes a native numeric
`CArray` the same way `Buf.allocate` does (zero-filled native storage), and a
reference-typed `CArray` (`CArray[Str]`, `CArray[Pointer]`, a CStruct element)
with that element type's ordinary gap value.
