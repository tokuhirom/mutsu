# `Proc::Async.stdout` is a real `Proc::Async::Pipe`

`Proc::Async.stdout` and `.stderr` now answer an instance of the class
`Proc::Async::Pipe`, rakudo's core subclass of `Supply`, instead of a plain
`Supply` that only smartmatched as a pipe. `.^name` is `Proc::Async::Pipe`,
`.WHAT` is `(Pipe)` and `.^mro` is `Proc::Async::Pipe Supply Any Mu`, as on
rakudo; the Supplies derived from a pipe (`.lines`, `.map`, ...) and the merged
`.Supply` stay plain `Supply`s.

The pipe is the same supply object underneath. The places that recognised a
supply instance by comparing its class name with `"Supply"` now ask one
predicate, `value::supply_class::is_supply_class`, and the method rows reach a
pipe through a `DispatchShape::Supply` alias, so tapping, `whenever`,
`.native-descriptor` and every Supply method work on it unchanged. The
attribute check that made a `Supply` instance smartmatch `Proc::Async::Pipe`
is gone: the type check now follows the class's MRO. Fixes #16667.
