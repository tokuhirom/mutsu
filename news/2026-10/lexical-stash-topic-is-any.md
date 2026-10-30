# `CALLER::LEXICAL::<$_>` answers `Any` when the topic is unset

A lexical pseudo-stash (`MY::`, `LEXICAL::`, `CALLER::LEXICAL::`) left `$_` out
or `Nil` until something topicalized it, so `CALLER::LEXICAL::<$_>.ord` returned
`Nil` instead of failing with `No such method 'ord' for invocant of type 'Any'`.
`$_` is a declared lexical of every scope, so the stash now always carries it
(`Any` by default). P5chr's `ord()` without arguments now dies like rakudo's.
