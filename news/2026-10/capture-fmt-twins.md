# `Capture` and `fmt` of the declined receivers are interpreter twins

The by-name `Capture` and `fmt` gates of the slow-path dispatcher are gone (ADR-11276 §9.187):
the interpreter's answers for a user object's or a live source's `Capture`, and for `fmt` with a
`Format` object or over objects with their own `Str`, are the late twins of `Mu.Capture` and
`Any.fmt`, reached once the pure rows decline. Answers are unchanged.
