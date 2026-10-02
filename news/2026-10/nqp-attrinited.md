# `nqp::attrinited` is implemented

`nqp::attrinited($obj, $class, '$!name')` used to die with
"Unsupported nqp:: op". It now answers whether an attribute holds something a
store or a read put there, rather than only the value construction seeded it
with — rakudo's notion, where MoarVM leaves such a slot null until the first
store or `getattr` vivifies it (ADR-0121 D4, #10957).

mutsu seeds every declared slot eagerly, so the "still null" state is kept on
the side: `AttrMap` carries one atomic seed bit per declared slot.
Construction sets it for an attribute no initializer wrote (no initializer and
no argument, the type object or native zero the parser synthesizes for a typed
scalar, an initializer deferred until after BUILD), and `nqp::create` sets it
for every slot. Every store clears it, and so does every user-level read — a
`$!x` read in a method, a generated accessor, `nqp::getattr` — as MoarVM's
vivification does.

Along the way, the attribute declaration records whether its default was
synthesized by the parser (`default_is_seed`), which also replaces the
RakuAST converter's guess that a `BareWord` default naming the declared type
was implicit; and the interpreter's `dispatch_new` constructor now lays its
instances out by the class layout like every other construction path.

Introspection reads (`.raku`, `Attribute.get_value`) do not yet count as
initializing (#11003).
