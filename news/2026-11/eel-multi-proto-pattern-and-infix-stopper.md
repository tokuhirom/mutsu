# EEL: `multi pattern` / `proto pattern`, and `<name: 4>` colon calls

EEL 0.3 declares its patterns with a slang `routine-declarator:sym<pattern>`, and
its suites write `multi pattern item(Int $n) { ... }`, `proto pattern item {*}`
and calls like `<choose: 4>`. Four gaps stood in the way:

- **A slang routine declarator after `multi`/`proto`/`only`.** rakudo's
  `multi-declarator` sets `$*MULTINESS` and parses `<declarator>`, so a slang's
  `routine-declarator` production competes after the keyword too. The parser now
  tries the slang's productions there, and the core `method-def`/`routine-def`
  the production calls declares a candidate, a `proto` or an `only` routine by
  that keyword. A `proto` body other than `{*}` is a statement list the slang's
  `statementlist` wrapper sees with `$*BLOCK` the proto.
- **`meta-object` of a `multi` method node** is the candidate that declaration
  installed (matched by its written parameters), not the dispatcher its name
  finds, so an action mixing a role into it marks the right candidate.
- **A role mixed into a proto's object** (`$proto does R`) now lands on the
  proto, where later lookups read it, instead of on its first candidate; and a
  `proto method` with no multi candidates is a dispatcher over none
  (`is_dispatcher` True, `.candidates` empty), as on rakudo.
- **rakudo's `infixstopper`:** while a slang's `arglist` override has
  `$*IN_REGEX_ASSERTION` set, a `>` ends the arguments instead of starting a
  comparison (`<choose: 4>` reads `4`). The host passes the dynamic with each
  back-call; a parenthesized expression keeps its comparison, as EEL's
  `circumfix` overrides reset the dynamic.

EEL's `t/20-proto-scenarios.t` now matches rakudo, and `t/19-proto.t` passes 18
of its 20 tests.
