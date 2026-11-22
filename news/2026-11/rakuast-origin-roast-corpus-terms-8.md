# RakuAST: an eighth set of roast-corpus origins, and Rakudo's `*` values

The `.origin` differential (#15780) found more nodes that were unplaced or
shaped differently from Rakudo. They now match rakudo 2026.11:

- **`*` as a value.** A parenthesized `*` (`say (*)`, `((*) + 1)`,
  `my $x = (*)`) and a bare `*` condition (`when * { }`, `if * { }`) are a
  `Term::Whatever` rather than a `WhateverCode::Argument` (#18688). Only a
  method call's target primes through one layer of parentheses (`(*).abs`).
  This classification is AST-only and does not change how a program runs.
- **`has @.a of int`** renders its type as a `Trait::Of`, as `my @a of int`
  already did. The parser records the `of` in the attribute's trait order,
  and lowering reads the type back from the trait.
- **A proto token's `{*}`.** Its `OnlyStar` is the empty range at the `}`.
- **`:sym` token names** (`token a:sym<foo>`, `:sym<<bar>>`, `:sym«<foo>»`)
  are placed together with their quote. A French-quoted symbol may now hold
  angle brackets and still be a `Name` with a `ColonPair::Value`.

Two differences are filed for later:

- `<sym>` inside such a token is a `Regex::Literal` on RakuAST rakudo, which
  sets no `$<sym>` (#18697).
- `((*) + 1)` still curries at runtime, while rakudo treats the parenthesized
  `*` as the `Whatever` value (#18698).
