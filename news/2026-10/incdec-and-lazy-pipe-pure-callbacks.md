# `$n++` and pure `$_ OP const` lazy-pipe callbacks stop going through the generic paths

Profiling #14370's two scripts showed neither cost was the "one-element pull" the
issue guessed at.

- `for (1..N).map({ $_ * 2 }) { $n++ }`: the pull is ~2.7k Ir per element, the
  body's `$n++` was ~3.3k. `PostIncrement`/`PreIncrement`/`--` took the by-name
  cascade (attribute sync, `our`/package lookups, `Symbol::intern`) even for a
  mainline `my $n` held in a slot. A plain, untyped, slot-held `Int` now steps in
  place (`exec_scalar_incdec_slot_fast`), declining for every shape the full path
  treats specially and still raising the readonly error. `$n++` goes from ~2.9k to
  ~1k Ir per step.
- `(1..*).grep({ $_ %% 3 }).map({ $_ + 1 }).head(N)`: ~55k Ir per produced element,
  almost all of it one closure call (~14k Ir: caller-env push, `Env` clone, frame
  push/pop) per callback in `fill_lazy_pipe`. A callback that is `$_ OP constant`
  over an Int (the shapes `pure_int_topic_expression` already admitted for the batch
  pull) is now computed by the operator itself, unless a user `infix:` is declared.
