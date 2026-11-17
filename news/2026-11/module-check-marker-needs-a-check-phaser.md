# Only a module with a CHECK phaser gets the end-of-compile-phase marker

Since a module's CHECK-time `$*CU` rewrite takes effect, a module's
statements carry a marker where its compile phase ends. A module without a
CHECK phaser got one too, right after its `use` lines. That broke Red: in
`unit role Red::Driver::Cache`, the `cache` routine found the `database` it
imports as a `proto` with no candidates, so every Red test that sets up a
cached driver died ("Cannot resolve caller database(Str:D)"; Red
`t/021-red-do`).

A module with no CHECK phaser has no CHECK phase that could rewrite it, so it
gets no marker again. The import bug the marker exposed in a `unit role`
file is tracked separately (#17962).
