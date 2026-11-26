# An EVAL'd unit has its own backtrace frame

`EVAL` ran its snippet on the caller's frame without a frame of its own, so a
`.backtrace` had no `EVAL_<n>` frame: the snippet's line was reported under the
caller, and under the caller's module file when a module routine called
`EVAL`. Only a `die`/`throw` happened to show `EVAL_<n>`, through their
synthetic leading frames. Running `EVAL`s are now recorded beside the routine
stack, and a backtrace splices in a `<unit>` frame for each, at the line within
the snippet, as rakudo does. A snippet that fails to compile now carries a
(compile-time) `.backtrace` too, instead of `Nil`.

Rakuish's REPL locates an error inside a cell by that frame; its
`t/82-errors-recall.t` goes from 84/88 to 87/88 (#19242). The remaining
assertion depends on further backtrace-shape differences, filed as #19244.
