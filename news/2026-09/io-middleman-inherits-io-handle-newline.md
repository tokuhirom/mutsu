# IO::MiddleMan can inherit IO::Handle's newline accessor

`IO::MiddleMan` 1.001004 is a pure-Raku `IO::Handle` subclass that wraps a
real handle and uses the inherited `nl-out` accessor while switching between
capture, mute, and normal modes. Its suite was only 3/10 files green under
mutsu: the other seven stopped at `Expected IO::Handle` before exercising
their assertions.

The native IO dispatch paths now recognize the inherited `nl-out` accessor on
an `IO::Handle` subclass and return its stored value, or the default newline,
without pretending that the subclass itself is a native handle. This keeps
user-defined wrapper attributes separate from the native descriptor layout.

The focused regression test `t/io/io-handle-wrapped-subclass-nl-out.t` pins the
inherited getter. The complete distribution suite now passes under both raku
and mutsu: 10/10 files and 24/24 assertions, with no remaining regressions.
