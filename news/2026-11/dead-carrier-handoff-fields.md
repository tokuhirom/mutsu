# Five dead carrier-compile side channels leave `Interpreter`

ADR-10779 D3 turns the `Interpreter`'s set-then-taken side channels into
parameters, one at a time. Five of them turned out to have nothing left to
convert: their other end had already been retired.

- `pending_supply_block_body`, `pending_supply_emitter_sym` and
  `pending_supply_authoritative_free_vars` carried a `supply { … }` body's
  marks into the carrier's re-compile of `SubData::body`. Nothing sets them
  any more; the carrier still took them on every block entry, applied the
  (always empty) marks to the freshly compiled chunk and kept them in the
  compile-cache key.
- `pending_eval_rw_tail` told the carrier re-compile of an `is rw` routine to
  compile its bare tail as a container. Nothing sets it any more: it was only
  ever taken, restored and cleared.
- `last_block_my_declared` published a carrier block's own `my` names for a
  `call_sub_value` exit merge that no longer reads it; every carrier block
  still built and stored the set.

They are gone together with the `CarrierPostCompile` mutation, its three
cache-key fields and the compiler's `set_rw_tail` setter. `Interpreter` drops
from 90 to 85 direct fields, and every carrier block entry skips three takes
and one `Vec` build.
