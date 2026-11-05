# Two per-call method gates are per name, not per program

With `use Cro::HTTP2::RequestParser` loaded, `$k.m(1)` cost 1.5x the bare
program (3.2 us against 2.1 us per call, release; 22.9K against 14.0K Ir). Two
process-wide "something exists somewhere" latches gated work on every method
call (#9170, same class as #16290 and #16309):

- ADR-0067's raw-invocant arrival gate: any method anywhere that declares a raw
  invocant (Cro has one) made every `$var.method(...)` resolve its method to ask
  whether it binds the invocant raw -- about eight symbol interns and an MRO
  resolve per call. The registration now sets a bit for the method's name in a
  lock-free 64K-bit set (`raw_invocant_method_possible_for`); the `CallMethod`
  and `CallMethodMut` gates test the called name's bit. A hash collision only
  runs the correct resolve.
- The sigilless-readonly probe: once any `my \x` readonly binding existed
  (a loaded module's), every whole-variable assignment built an
  `__mutsu_sigilless_readonly::<name>` key and probed the env for it.
  `note_env_key` now also sets the marker name's bit, and `CheckReadOnly` /
  `check_named_incdec_readonly` test it first.

Callgrind slope (release, warm, 2000 -> 20000 calls): loaded 22.9K -> 19.6K Ir
per call (bare 14.0K); wall clock 1.5x -> 1.25-1.3x of the bare program.
Pinned by `t/oo/method/raw-invocant-gate-is-per-method-name.t` and the existing
ADR-0067 raw-invocant tests.
