# mutsu's structured exceptions now carry the rakudo attributes `throws-like` was skipping

Split out of the `throws-like` vacuous-matcher fix
(`news/2026-08/throws-like-named-matchers-no-longer-silently-skipped.md`).
That fix restored every named matcher whose value mutsu could actually
produce; ten more could not run at all, because the exception carried no such
attribute — or, in half the cases, because mutsu never raised the right
exception type in the first place. Those were reported out loud rather than
silently dropped:

```
# SKIPPED matcher '.multiness': mutsu's X::Anon::Multi carries no such attribute
```

**All ten are now real, passing assertions — 18 `SKIPPED matcher` notices
across four roast files became zero.** Every expected value was read off
`raku`'s own metamodel (`.^attributes`, then the attribute in a concrete
throwing case) before any interpreter change; nothing was invented to satisfy
a matcher.

## What each one turned out to be

Two kinds of gap, in roughly equal measure.

**A. The right exception, missing attributes.** These were derive-or-thread
jobs.

- **`X::Anon::Multi.multiness` / `.routine-type`.** rakudo's message *is*
  `An anonymous {routine-type} may not take a {multiness} declarator`, so both
  attributes are now derived from the text in
  `RuntimeError::exception_value` — the same derive-don't-duplicate rule
  already used for `X::Syntax::Missing.what` and `X::InvalidType.typename`,
  and it covers all three parser sites that raise this diagnosis at once. The
  message itself was wrong for one case: `anon_multi_check` always said
  "routine", so `class { multi method () { }}` reported `routine-type` of
  "routine" where rakudo says "method". It now tracks the declared routine
  type.
- **`X::Method::InvalidQualifier.method` / `.invocant` / `.qualifier-type`.**
  These *cannot* be re-derived from the message, which only holds the two type
  names: rakudo's `.invocant` is the offending VALUE (`1`, matched as
  `invocant => 1`) and `.qualifier-type` is the TYPE OBJECT (matched as
  `qualifier-type => List`). All three qualified-dispatch throw sites in
  `runtime/methods_qualified.rs` now go through one
  `RuntimeError::invalid_qualifier` builder that threads the real invocant and
  builds the qualifier's type object, keeping the previous message text
  unchanged.
- **`X::Trait::Invalid.type` / `.subtype`.** `sub foo($x? is rw) {}` raised the
  class with nothing but a mutsu-invented message ("Cannot make an 'is rw'
  parameter optional"). rakudo splits the trait into `.type` ("is") and
  `.subtype` ("rw"), plus `.declaring` and `.name`, and composes its message
  out of exactly those four: `Cannot use 'is rw' on optional parameter '$x'.`
  Both are now stored and the message matches.
- **`X::Syntax::Variable::Numeric.what`.** `my $0` was diagnosed but carried no
  `.what`; the signature twin `sub f($0) { }` was not diagnosed at all (the
  parser merely failed to find a name, and the whole declaration came out as
  the generic `X::Syntax::Confused` parse-error blob). A `reject_numeric_param`
  check now mirrors the `my $0` one, and the two are distinguished by `.what`
  ("variable" vs "parameter"), as rakudo does.

**B. The wrong exception entirely.** These needed a real parser diagnosis,
and each fixed a genuine misparse rather than just a label.

- **`X::Syntax::Number::RadixOutOfRange.radix`.** `:45<abcd>` backed out of the
  radix-literal parser *softly*, so every other alternative was tried and the
  failure surfaced as `X::Syntax::Confused`. The `<` is what makes `:NN<...>`
  unambiguously a radix literal, so an out-of-range base is now a definite
  diagnosis carrying `.radix`. Its run-time twin (`"z".parse-base(45)`) already
  had the type and the attribute but spelled an *older* rakudo message; both
  now share one builder and rakudo's current wording,
  `Radix 45 out of range (allowed: 2..36)`.
- **`X::Syntax::Missing.what` for `constant * = 3;`.** The declarator is
  committed once `constant` is read, so a name that is not an identifier is a
  definite diagnosis — rakudo reports the same `X::Syntax::Missing` it uses for
  `constant foo;` (its grammar never gets past the name to the `=`). `.what`
  was also too short: rakudo's `X::Syntax::Missing` message is `Missing {what}`,
  so `.what` is the whole tail, `"initializer on constant declaration"`. That
  is the only spelling under which *both* of roast's regexes
  (`what => /initializer/` and `what => /constant/`) match this one diagnosis.
- **`X::Syntax::DuplicatedPrefix.prefixes`.** mutsu already had the builder and
  the attribute; two rakudo cases simply never reached it, and both were real
  misparses:
  - `1%^^1` — `could_start_var_name` accepted a bare twigil character after a
    sigil, so `%^^1` was read as a *variable* and the `%` never became the
    modulus infix. rakudo reads `1 %^1` as `1 % ^1` and `1 %.5` as `1 % .5`: a
    twigil is only a twigil when a name follows it. The predicate now takes the
    text after the sigil and requires one (leaving `%=` / `%~` alone, since
    those guard the compound-assignment spellings rather than naming twigils).
  - `555 ~~!~~ 666` — `parse_prefix_unary_op` deliberately refuses to take the
    `!` of `!~~`, so that the infix `!~~` survives at infix position; that also
    meant `prefix_expr` could not recurse past it to see the `~~` run. It now
    looks through a single leading `!` and reports the run itself
    (`prefixes => "~~"`, as rakudo does).
- **`X::Syntax::CannotMeta.meta` / `.operator` / `.reason`.** `$a R[and]= 42`
  is a metaop letter over a bracketed infix *looser than assignment*. `[OP]=`
  only composes into an assignment metaop when `OP` is tighter than assignment,
  so the `=` stays a plain assignment operator and the metaop letter has
  nothing but that `=` left to meta — which rakudo refuses by name:
  `Cannot reverse the args of = because assignment operator operators are too
  fiddly`. Checked in the list-infix loop before the metaop scanner takes
  `R[and]` as a plain infix and strands the `=`. Verified against rakudo for
  `and`/`or`/`andthen`/`orelse`/`xor`/`,`, for all four metaop letters, and for
  the boundary: the tighter `R[+]= 42` really does compose and still parses.

## Pin

`t/exception-attributes.t` asserts every attribute added here through a
`throws-like` attribute matcher — the very path the parent fix restored — and
it passes under both `raku` and mutsu. It also runs the **failing** direction
in a child process, asserting that each subtest plans `1..3` (a skipped matcher
plans `1..2`) and that a wrong `.multiness` / `.radix` / `.qualifier-type`
really emits `not ok 3`. A test that only checked the passing direction would
be exactly as vacuous as the bug this descends from, since a skipped matcher
"passes" too.

`roast/S32-exceptions/misc2.t`, `roast/S03-metaops/reverse.t`,
`roast/S03-operators/misc.t` and `roast/S06-signature/optional.t` were already
whitelisted (their vacuous subtests passed); they now assert what they claim
to.

## Still open

`$!.backtrace` for a compile-time diagnosis is a `Str`, not a `Backtrace`, so
`.is-runtime` cannot be asked of it at all. Tracked separately in
`todo/tickets/compile-time-diagnosis-backtrace-is-a-string.md`.
