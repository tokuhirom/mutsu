# A resolved `.then` callback's `die` no longer reaches the caller's `CATCH`

`.then` / `.andthen` / `.orelse` on an already-resolved promise runs its
callback inline on the calling thread. A `die` inside that callback was handled
inline (ADR-0072 throw-site `CATCH`) by the `CATCH` of the frame that merely
registered the callback, and then a second time when the broken derived promise
was awaited. `throws-like { await $p.then({ die ... }) }` therefore reported
"planned 2, ran 4" whenever the source promise happened to resolve before
`.then` was called — which made Cro::Core's `message-with-body.rakutest`
fail intermittently under load in the battery gate.

The inline callback now runs with the caller's `CATCH` regions set aside, as
Rakudo always cues it on the scheduler: its failure only breaks the derived
promise.
