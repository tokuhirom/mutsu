# An `@` parameter de-itemizes an Array that arrives without a holder

`sub f(@a) { @a.map(...) }` called with `%h.AT-KEY('a')`, `%h.values[0]` or an
element of a `Map` subclass built over a shared `$!storage` bound the itemized
Array as one element, so `@a.map` ran once over the whole Array. The general
binder now de-itemizes any `@` parameter's argument, as the plain-variable path
already did, so the elements become the parameter's elements. This unblocks
JSON::RepositoryEvent's `bless-array-elements-as` (RepositoryEvent
`t/02-push.rakutest`) (#16357).
