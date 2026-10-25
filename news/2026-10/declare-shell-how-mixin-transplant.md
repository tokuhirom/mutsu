# A nested DECLARE'd class takes its shell's HOW mixin, not its HOW state

A declaration nested in another one (Red's `model Ble is table<...> { }` inside `model Bla { }`) is
registered by a compile-time shell and again in place; the traits run in the shell only. Keeping the
shell's whole metaobject for the in-place registration (so that a role a trait mixed into it,
`$type.HOW does R[Arg]`, survives with its arguments) also kept the state the shell's `compose` had
left in it, so the nested model's columns and relationships were registered twice and Red's
`t/61-model-as-type` returned the wrong model's rows. The in-place registration now installs a fresh
metaobject and moves only the shell's mixin onto it, role arguments included.
