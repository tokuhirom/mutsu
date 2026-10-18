# `List.push` and friends on the type object raise `X::Immutable`

`List.push`, `append`, `unshift`, `prepend`, `pop` and `shift` called on the `List` type object
raised `X::Method::NotFound`. The rows carry `RowFlags::IMMUTABLE` now, so the type-object refusal
step answers Rakudo's `X::Immutable` (ADR-11276 §9.177).
