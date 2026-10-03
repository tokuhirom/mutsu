# A worker thread's `@a.push` is visible to a lexical sub reading `@a`

When a closure and a named sub both captured one `my @a`, and the closure
pushed onto it from a `start` thread (or from a Supply fed by one), the named
sub kept seeing the old contents on that thread (#11238). On a worker, a
push goes into the cross-thread atomic lane, and every by-name read prefers that
lane. The named sub reads `@a` through its lexical-sub alias cell instead, which
was found first and only ever reflected the main thread's view.

On a worker thread, an `@`/`%` read through a lexical-sub alias now consults
the atomic lane first, the same order the by-name reads use. This was the
`@order` queue in Cro::WebSocket's `MessageSerializer`, whose `sub set-current`
saw an empty queue and never produced the message frame. Its serializer test
went from 6 failing checks to 3. The rest is a separate issue: emits from two
threads in one `supply` block reach the tap concurrently (#11307).

Two follow-ups were filed: #11306, the lane is keyed by name so two unrelated
same-named arrays collide (this predates the change), and #11307.
