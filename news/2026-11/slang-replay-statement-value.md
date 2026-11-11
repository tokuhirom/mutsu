# A slang production's value and a slang-built `my` method survive the action replay

A slang production whose actions are replayed in the unit (ADR-17243) lowers
to its own statement plus the replay call. EEL's `pattern` declarator moved
the replay after the production, so the replayed action sees the method it
declared. That made the replay's `Nil` the value of every expression
production. Qwiratry's `givenroot $data { root.elems }` returned `Nil` as a
routine's last statement. Only a declaration now runs before its replay. As in
rakudo, where actions run at compile time, before any statement does, any other
production runs after the replay and keeps its value.

The same change looked up an owner-less slang-built method in the class whose
body holds the production, for `meta-object`. A `my`-scoped method (Qwiratry's
`mold ... do { }`) is in no class's method table, so `meta-object` came back
undefined. Such a method now gets the node's own method object again, as before
(#17460).
