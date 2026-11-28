# Method bodies with `**`, `~` or `return` run in the slot frame

The slot method frame (ADR-18393) runs a planned method without an env overlay, by-name parameter copies or a fat call-frame record, for about 1.5k instructions of frame work per call. The lean frame it replaces costs about 6.7k. Until now its op list admitted only slot reads, constants, `+ - * /` and zero-argument calls on a parameter, so very ordinary bodies fell back to the lean frame:

- `(($!x - $o.x) ** 2 + ($!y - $o.y) ** 2).sqrt`, because of `**`;
- `return $.name ~ " has " ~ $.legs ~ " legs"`, because of `~`, the item wrap around a `$.x` read, and `return`.

The list now also takes `**`, `%`, `div`, negation, the numeric and string comparisons, `~`, `Itemize` and `return`. The operators read and write no name: a user candidate or a `.Str` coercion they call builds its own frame over the caller's env, as for `+`. `return` is caught by the body loop the lean frame shares. Its one by-name read, a lexically rebound `&return`, sends the call back to the lean frame through a per-call check.

A zero-argument call on `self` (`self.describe()`) stays out of the slot frame. The slot body would run it as a `CallMethod`, which has no method-call plan lane yet and costs more than the frame saves.

Callgrind slope per loop iteration, profiling build:

| input | before | after |
| --- | ---: | ---: |
| `$p1.distance-to($p2)` (method-call's body) | 21.3k Ir | 17.5k Ir (-18%) |
| bench-class's `full-info` + `bark` iteration | 46.8k Ir | 43.3k Ir (-7%) |
