# Gauge: a role composing its core namesake, nested `It(...)` calls, `:_` pseudo-invocants, `**...**`

Four gaps found loading the Gauge distribution:

- `role Iterator does Iterator` inside `unit class Gauge` composes the core `Iterator` instead of
  failing as "cannot compose itself"; the self-composition refusal moved from the parser to
  registration, which can see the core roles.
- `It($x)` in a method of `class Gauge` calls the nested `Gauge::It`'s `CALL-ME`: the call-position
  type lookup now also walks the running routine's own package chain.
- An invocant `::?CLASS:_:` survives the RakuAST round trip.
- `(**...**)` parses: `**...` was read as a power operator.

Gauge itself still needs #15265 (`Seq` subclass constructor) and #15266 (`$*VM.version`).
