# A whenever over a quitting materialized supply runs its QUIT phaser under tap and list

`.tap` (and so `.list`) on a `supply` block whose `whenever` source is a
materialized supply that quits (`Supply.from-list(1, 2).map({ die })`) dropped the
quit, so the `QUIT` phaser's `emit` was lost and `.list` answered `()`. The cold
whenever replay now carries the source's `quit_reason` into the phaser, as the
`.Promise`/`await` path already did: the snippet answers `(bang)` as in Rakudo.
