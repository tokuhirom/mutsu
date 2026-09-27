# DateTime::Format exposes a formatter gap in DateTime.utc

DateTime::Format 0.1.5 passes its metadata and strftime tests under mutsu. Its
RFC 2822 test still fails after converting a formatted DateTime with `.utc`:
Rakudo retains the formatter and prints the RFC 2822 form, while mutsu falls
back to its ISO form. The missing behavior crosses DateTime's native method
dispatch and the interpreter-aware rendering used by string coercion, so the
general capability is tracked in [#9890](https://github.com/tokuhirom/mutsu/issues/9890).
