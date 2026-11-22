# `Supply.delayed` over a live source no longer blocks the emitter

rakudo's `delayed($time)` cues each value on the scheduler (`$scheduler.cue:
{ emit val }, :in($time)`), so `$supplier.emit` returns at once and the taps
see the value `$time` seconds later. mutsu slept for the delay inside the
emitter before calling the tap synchronously, so `emit` blocked the caller and
the value was already delivered when it returned (#18608).

A delayed live supply is now a per-tap stage like `stable` and `throttle`: a
worker holds each source value until it is due and emits the values in arrival
order. `done` is passed on after the last pending value, and `quit` drops the
pending values.
