# A sigilless name bound to a Map refuses assignment

`my \m = Map.new((a => 1)); m = 3` used to rebind `m` to `3`. A sigilless
declaration decides whether its name denotes a writable container from the
value it binds, and every Hash counted as one — including an immutable `Map`,
which then also skipped the in-place STORE a mutable Hash gets, so the plain
store overwrote the name. An immutable Map now counts as a value, and the
assignment dies with rakudo's `X::Assignment::RO`. The message of that
refusal now renders the value with `.gist`, as rakudo does
(`Cannot modify an immutable Map (Map.new((a => 1)))`), and carries the
`value` attribute. (#11362)
