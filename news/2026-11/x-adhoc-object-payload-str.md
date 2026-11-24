# `X::AdHoc` with an object payload renders through its `Str`

`X::AdHoc.new(payload => F.new).message` returned the payload object itself,
so a class with a `Str` method printed `F<21>` instead of its text; the same
held for `.Str`, `.gist`, an uncaught `.throw` and `Failure.new(F.new)`.
rakudo's message is `$!payload.Str`. These now call the payload's own `Str`,
user code included, and a `Str` that dies makes `message` die, as in rakudo
(#19083).
