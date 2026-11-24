# `dd` shows what `.raku` answers

rakudo's `dd` is `note prefix ~ $var.raku`. mutsu rendered the value part with
the static `.raku` renderer, which knows neither a typed Array's element type
nor a Block's signature. So `dd` printed `Int  = [1]` where `.raku` gives
`Array[Int].new(1)`, and an empty value for a Block (#19047).

A direct `dd` call now compiles each argument together with its `.raku`, an
ordinary method call the VM dispatches. The builtin prints that text, so a
user `raku` method is honoured too. This also replaces the runtime
method-dispatch fallback the builtin used for class instances. As on rakudo,
`dd` returns `Nil`. A `dd` the program declares or binds still takes the call.
`dd` called as a routine value (`&dd`) keeps the static renderer.

The `dd` builtin moved out of `builtins_eval_misc.rs` into its own file.
