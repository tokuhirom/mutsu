# `Supply.new` over a user `Tappable`

`Supply.new($tappable)` threw `X::Supply::New` for every argument, while
rakudo builds a supply over any object that does `Tappable` and calls its
`tap(&emit, &done, &quit, &tap)` for each subscription. EEL 0.3's
`Supply.match` returns such a supply (`MatcherSupply`, #16888). mutsu now
carries the tappable as an on-demand supply whose producer forwards the
emitter to the tappable's `tap`, so every Supply method and `whenever` drives
it. Without a tappable, `Supply.new` still throws `X::Supply::New`.

A method's `&done` / `&emit` parameter now also shadows the keyword of the
same name inside the method body, as a sub's already did: `done()` in
`method tap(&emit, &done, ...)` called the react-block `done` before.
