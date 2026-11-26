# Regex validation no longer builds a second interpreter

The parse-time regex validator used to borrow a whole thread-local
`Interpreter::new()` as the receiver for the structural regex parser, so every
program containing a regex literal paid for a second full start-up (~0.9M
instructions on `say "hello" ~~ /ell/`).

The structural parser is now a value of its own, `RegexParser<'a>`, which
carries the runtime as an `Option<&Interpreter>`: a `Match` parse gets the
running interpreter, a parse-time `Validate` gets `None`. The runtime questions
the parser asks (the current package, whether the grammar defines a token) are
helper methods that answer as an empty runtime would when there is none, so a
runtime read during validation cannot be written by accident. The
`VALIDATE_INTERP` thread-local is gone, and the interpreter-construction
allowlist shrinks by one site.

Measured on a release build (`MUTSU_JIT=off`, callgrind): `say "hello" ~~ /ell/`
drops from 8.39M to 7.49M instructions (-10.7%). Refs #19197.
