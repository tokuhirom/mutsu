# A subscript chain hands vivifying methods a location

`%h<a><b>.push(1)`, `$x{k}.push(1)` over an undefined scalar (captured or not),
`@a[1][2].push(3)` and the other vivifying calls now compile their receiver as the
deferred-location chain a `:=` bind to a subscript uses, ending in
`IndexInvocantRef { vivify }`; `CallMethod` then stores the fresh container through the
location (`ReceiverPlace::Cell`, ADR-13823). The name-based `IndexElemAutoviv` path loses its
`autoviv` mode and the `pop`/`shift`/`splice` temp binding (`__mutsu_tmp_index_method_target_N`)
is gone: those methods never vivify and are plain method calls on the element. Only attribute,
dynamic and package scalar bases (`$!x{k}.push`) still use the name-based path. `f().push(1)` and
a read-only accessor's value die with `X::Assignment::RO`.
