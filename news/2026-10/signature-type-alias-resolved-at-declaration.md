# A signature's type alias names the aliased type

Upstream `NativeCall` exports its C types as constants aliasing the real
declarations (`my constant size_t is export = NativeCall::Types::size_t`).
mutsu kept such a spelling in a parameter type or `--> T` as written, so the
routine's types were later looked up by the alias name wherever they were
asked for. Read inside the `NativeCall` module, where the importer's alias is
not in scope, `--> size_t` answered an unrelated `size_t` with REPR
`P6opaque`, and upstream's `check_routine_sanity` warned about an erroneous
return type and rejected `size_t` parameters (#11555, part of #11203).

A routine's registration now rewrites each alias spelling in its signature
to the aliased type's name while the declaring scope is live, as rakudo
stores the type object itself. The in-sequence registration replaces the
twin the hoist pass installed before the `use`/`constant` binding the alias
had run. Alongside:

- a `native`-declared type binds a value of the type its REPR boxes to
  (`size_t $n` takes an Int), while smartmatching a value against it stays
  False, as for a core native;
- the native marshaller maps such a type to the core native with its layout
  (`NativeCall::Types::size_t` marshals as `uint64`).

Follow-ups: #11706 (a lowercase imported `--> name` is compiled as a definite
return value) and #11707 (a declared native does not unbox/wrap its argument).
