# An attribute twigil inside a regex is rejected at compile time

`class C { has $.a; sub f { my token t { $!a } } }` compiled cleanly in mutsu.
Rakudo rejects it with `X::Attribute::Regex`: "Attribute '$!a' not available
inside of a regex, since regexes are methods on the Cursor class". The
structural regex validator only ran on regex *literals*, so a
`token`/`regex`/`rule` declaration body was never checked (#10544).

- A declaration body is now validated for this error at parse time, in a
  grammar or in any routine.
- `@!attr` is rejected like `$!attr`. It used to be reported as an
  unrecognized `!` metacharacter.
- A code block is rejected only when one of its statements is a bare
  attribute (`{ $!a }`, `{ 1; $!a }`), as rakudo does. Using the attribute
  inside a larger expression (`{ say $!a }`) now works; mutsu used to reject
  any `$!` anywhere in the block.
- The message now matches rakudo's.

Pinned by `t/regex/regex-attribute-twigil-rejected.t`.
