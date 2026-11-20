# `Supply.squish` and `Supply.unique` follow a live or on-demand source

`Supply.squish` read its source's materialized values and built a static
Supply from them. Over an on-demand `supply { whenever $live { ... } }` that
meant running the block to completion, which never happens while the
`whenever` follows a live supply, so `.squish` hung; over a Supplier-backed
source the snapshot was empty and later values never arrived. `Supply.unique`
already followed a Supplier-backed source but still snapshotted an on-demand
one, so `supply { emit 1; emit 1; emit 2 }.unique` was empty.

Both now follow the source per tap, as rakudo's `supply { whenever self { ...
} }` does: a Supplier-backed source gets a squish-aware variant of the
existing `unique` transform tap, and an on-demand or channel-backed source
gets a derived on-demand supply (new `unique` / `squish` modes of the
`supply_derive` machinery) that keeps each tap's seen keys, honouring `:as`
and `:with`.

Red 0.2.14's `ResultSeq.Supply` ends in `supply { whenever $live { emit } ...
}.squish(:as(&supply-identity))`, and its `t/190-resultseq-supply.rakutest`
now passes (#18400, found under #18386).
