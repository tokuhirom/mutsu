# `Proc::Async.stdout` / `.stderr` are `Proc::Async::Pipe`s

`Proc::Async.stdout` and `.stderr` answer a `Proc::Async::Pipe` in Rakudo
(a `Supply` subclass); mutsu answered a plain `Supply`, so a signature or
return type naming `Proc::Async::Pipe:D` (Shell::DSL's `Command.stdout`)
failed its type check. The two streams now carry a `proc_pipe` marker that the
type matcher reads, so `$p.stdout ~~ Proc::Async::Pipe`, the `:D` / `:U`
smileys and parameter / return constraints work. The merged `.Supply` and
Supplies derived from a pipe stay plain Supplies. `Proc::Async::Pipe:D` also
resolves as a bareword type with a smiley.

Not done: `.^name` of such a stream still says `Supply`, and
`Proc::Async.Supply(:bin)` still delivers `Str` (tracked on #15647).
