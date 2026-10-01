# Regex values have per-evaluation identity

A Raku regex is a `Code` object: every evaluation of a regex literal or an
anonymous `regex`/`token`/`rule` term creates a distinct object. mutsu loaded a
capture-free literal with `LoadConst`, so every evaluation shared the constant's
payload, and `===` on regexes compared pattern text — `(^2).map({ /a/ })` gave
two `===` values, and so did `/a/ === /a/`.

Now every regex literal in value position loads through `LoadRegexClosure`,
which mints a fresh payload per evaluation (`Value::fresh_regex_code_object`,
one allocation: the payload's source tree became an `Arc`, so nothing is deep
copied). `===` compares regexes by payload, and `.WHICH` (and the object-hash
key) reports a never-reused `RegexId` drawn per payload, so a `.WHICH` string
that outlives its regex cannot collide with a later one. `eqv` stays
structural, as in Rakudo (`/a/ eqv /a/` is `True`). The adverbed form
(`rx:i/a/`) was already one shared `Arc` payload per value inside the NaN box,
so it gets the same identity.
