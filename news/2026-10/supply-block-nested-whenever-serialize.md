# A supply block's nested `whenever` bodies are serialized with their siblings

Raku runs one `whenever` body of a `supply` block at a time, and the block's
`emit`s reach its tap inside that body, so tap callbacks never overlap. mutsu
held the block's serialize lock only for `whenever`s registered when the block
was tapped, keyed on their source supplier. A `whenever` registered later from
inside a body — for example one over a nested supply block that is fed from a
`start` thread — ran beside a sibling body on another thread, and two tap
callbacks ran at once.

`call_supply_tap` now holds the serialize group of the block a stamped
`whenever` callback belongs to for the whole call, so every `whenever` body of
a block, however it was registered, takes the same re-entrant lock.
Cro::WebSocket's `t/websocket-message-serializer.rakutest` ("Control message
in-between") now passes 29/29, as under rakudo (#11307).
