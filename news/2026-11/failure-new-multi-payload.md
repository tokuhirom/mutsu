# `Failure.new` accepts a list of payloads

`Failure.new(now.DateTime, 'WELP‼')`, the example in raku-doc's `Failure`
page, kept only its first payload. A `Failure.new` with two or more payloads
now builds its exception through `X::AdHoc.from-slurpy`, as rakudo's
`|cap (*@msg)` candidate does: the message concatenates every payload and the
payload is the `Capture+{X::AdHoc::SlurpySentry}` capture. `X::AdHoc.message`
also renders a list payload as its `Str` (`a b`) instead of its gist
(`(a b)`) (#18984).
