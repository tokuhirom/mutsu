# `last` in a `deepmap` / `nodemap` block

`@list.deepmap({ last if ...; ... })` died with an uncaught `X::ControlFlow`.
The Array and Seq arms of `deepmap` and `nodemap` now treat `last` as rakudo
does: it ends the iteration of its own level, a nested list ending only
itself. (`Hash` receivers still differ from rakudo's odd empty-result
behaviour.) ParaSeq's `t/49-nodemap.rakutest` and `t/47-deepmap.rakutest`
no longer die at load.
