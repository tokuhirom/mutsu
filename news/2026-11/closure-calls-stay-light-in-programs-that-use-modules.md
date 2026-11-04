# A trivial closure call no longer costs 4-18x more once a module is loaded

The light closure call (#8335) runs `-> $x { $x + 1 }` with only what the body can observe, ~3.4K
instructions a call. Two process-wide conditions switched it off for every closure of a program
that loaded certain modules, sending each call through the general closure entry (~40K instructions,
it builds a fresh env tier per call):

- Any `my atomicint` anywhere in the process (Cro declares one) turned off the slot-read latch the
  light call consults. That fact is per name, as `GetLocal`'s own fast path already treats it
  (#12120): only a closure whose own parameter could be an atomic variable declines now.
- Every built-in operator of a closure in a unit with a `use` carries a `LexImportBuiltinOp` prefix
  (ADR-8751 stage 2), which the light body shape did not admit. The prefix does nothing unless an
  import set of the body's own chain binds that operator, so the light call now admits it and asks
  that same question per call (`light_body_has_live_operator_import`).

100000 calls of one trivial closure, release build, after the `use`: no module 33-45 ms (unchanged);
`use JSON::Fast` 194 -> 41 ms; `use DBIish` 672 -> 44 ms; `use Cro::HTTP2::RequestParser`
616 -> 114-128 ms. Pinned by `tests/name_resolution_stats.rs` (a light call chains no overlay) and
`t/routines/closure/light-closure-call-beside-use-and-operator-import.t` (#9170).
