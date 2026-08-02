# `.new` ignores a named argument that names no attribute

Raku's default `BUILDALL` only initialises **declared** attributes. A named
argument to `.new` that names none is silently ignored — it never becomes part
of the object. mutsu stored it in the instance's attribute map instead. The key
was invisible to `.^attributes` and to attribute access (`$!bogus` still failed
to compile, `.^can` still returned nothing), which is why it went unnoticed for
so long — but the instance equality behind `eqv` / `===` compares the raw
attribute map, so an object carrying a stray key never matched an otherwise
identical one:

```raku
class C { has Int $.x }
say C.new(x => 1) eqv C.new(x => 1, bogus => 2);   # raku: True    mutsu: False
```

Construction now drops such an argument, at all three sites that fold named
args into the map: the native default-constructor fast path
(`build_native_default_instance`), the interpreter's generic `dispatch_new`, and
`dispatch_bless`.

## Why it is gated

Dropping unconditionally would break mutsu's **built-in attribute-bag classes**.
`X::AdHoc.new(payload => …)`, `X::TypeCheck::Binding.new(got => …, expected =>
…)` and every other built-in exception type are registered with an *empty*
attribute list and rely on the permissive bag; a user class with a built-in base
(`is Exception`, `is Supplier`, …) inherits attributes — `message`, `payload` —
that the registry does not know about either.

So the drop is gated on a new memoised `NativeCtorPlan::attrs_fully_known`: the
class declares at least one attribute, and every type in its MRO other than the
universal roots (`Any`/`Mu`/`Cool`) is a registered class **or role** (role
attributes are flattened into `ClassDef::attributes` at composition time, so a
known role is as complete as a known class). `Exception` anywhere in the MRO
disqualifies the class outright. Being part of the ctor plan, the check costs one
already-cached bool per construction.

## Effect

Upstream `Cro::HTTP2::FrameParser` splats a header hash carrying
`conn => $packet.connection` into every frame class, none of which declares a
`conn` attribute:

```raku
Cro::HTTP2::Frame::Data.new(padding-length => ($padding-length // UInt),
                            data => Buf.new($payload), |%header);
```

Every parsed frame therefore carried a stray `conn` key and compared unequal to
the frame the test built by hand, so `is-deeply $frame, $result` failed with two
identical-looking gists on both sides. That was the remaining half of
`t/http2-frame-parser.rakutest`'s failures, and the same `|%header` splat feeds
`Headers`, `Priority`, `RstStream`, `Settings`, `PushPromise`, `Ping`, `GoAway`,
`WindowUpdate` and `Continuation`.

Pin: `t/new-ignores-undeclared-named-args.t` (also passes under `raku`).
