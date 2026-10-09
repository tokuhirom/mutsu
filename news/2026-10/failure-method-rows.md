# `Failure`'s accessors are rows

`exception`, `handled`, `gist`, `raku`, `Str` and `Bool` of `Failure` are rows of
the method table now (ADR-11276 §9.52). The scattered `Failure` arms of the
zero-argument cascade and the repr dispatch call one handler per method.
