# `split` is found through the receiver's MRO

The by-name `split` arm of the slow-path dispatcher, which sent every receiver to one routine, is
gone (ADR-11276 §9.182): `split` joins the text methods answered by walking the receiver's MRO to
the first owner that declares the row, so a grammar cursor, a `Cool` subclass and a `gather` of
needles reach `Cool`'s and `Str`'s rows, and `IO::Path`/`IO::Handle` their own.
