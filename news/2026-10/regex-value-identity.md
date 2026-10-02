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

The same shared payload gives every regex value a name cell for
`Code.set_name`: `rx:i/a/.set_name('q')` now renames the value as seen through
every alias, and renaming one evaluation of a literal no longer leaks into the
next. `Value::regex` (the runtime's synthesized regexes) builds the closure
payload too, so no regex value is left without a name cell.
