# A `gather` returned from a `.map` block is no longer rendered as empty

`(1,).map({ gather take 5 })` used to leave an unforced gather `LazyList` as
the element of the mapped result. Every reader that cannot run the VM — gist
and `.raku` rendering, `.flat`, array and hash assignment — saw nothing, so it
printed `()` instead of rakudo's `((5))` (#9584; found through Getopt::Long's
`@!options.flatmap(&to-receivers)` in App::Lorea).

`reify_finite_pipe_value`, which already reified a finite `.map`/`.grep` pipe
returned from a callback, now also gives a plain (non-`lazy`) gather a bounded
pull. A gather that finishes within the bound becomes a reified `Seq`; one that
does not (`gather loop { take 1 }`) keeps its cached prefix and suspended
coroutine, so `(1,).map({ gather loop { take 1 } }).head.head` still answers
`1` instead of hanging. The lazy map pipeline (`(1..*).map({ gather ... })`)
reifies callback results the same way. The helper moved to its own file,
`src/vm/vm_helpers_lazy_reify.rs`.
