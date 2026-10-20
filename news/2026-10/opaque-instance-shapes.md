# Opaque instance shapes: the runtime's own classes reach their rows through the table

An instance of a built-in class whose state lives in the interpreter or in its
attributes (`Lock` and its kin, `Semaphore`, `Thread`, `Tap`, `Proc`, the
sockets, `IO::CatHandle`, the schedulers, the encodings, the `CompUnit` and
`Distribution` family, `WalkList`, `Routine::WrapHandle`,
`HyperConfiguration`, `Pod::Block::Declarator`, `Supply`, `Proc::Async`,
`Promise::Vow`), and the value kinds `Promise`, `Channel`, `HyperSeq` and
`RaceSeq`, now have an *opaque* dispatch
shape (ADR-11276 slice 5). The shape names the owners its rows live under, so
the method table answers it from every call entry through the one guard step,
and the per-class `owners_of` hooks those call sites asked (`handle_owners_of`,
`runtime_object_owners_of`, `compunit_owners_of`, `cathandle_owners_of`, the
schedulers', the encodings', `supply_owners_of`, `hyper_seq_owners_of`,
`promise_channel_owners_of`) are gone. A call the guard step declines finds
its row through the shape's owners at one place in the by-name dispatch.

An opaque shape reaches only its own class's rows: what the class does not
declare (`gist`, `Bool`, the list methods) is answered as before, and the
shape is invisible to every protocol that reads `Value::dispatch_shape`.

To make room, the table keeps a `u128` bit per shape (up to 127 shapes), and a
shape is found from an instance's class or a type object's name with one hash
lookup instead of a linear scan. A new row flag, `EFFECTS`, marks a row whose
handler needs no interpreter but changes state (a decoder's byte queue, a
cancellation): no debug cross-check runs its call twice.
