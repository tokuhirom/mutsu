# `self.bless(|%_)` skips the method-call probe chain

A user constructor is almost always `method new(*%_) { self.bless(|%_) }`.
Its `self.bless(...)` is a `CallMethodMut`, which walked the argument stretch
and the long chain of speculative probes (proto bodies, exception delegates,
lazy lists, autothreading, storage delegates, the native method table, ...)
before reaching the native `bless`, on every construction.

The new bless lane is the default-constructor lane's twin: the first
`self.bless(named...)` on a class's type object that walks the whole chain into
the native `bless` records the class (reaching it is the proof every probe
declined), and later calls go straight there from the top of the opcode. The
early replay runs the call site's own `|` spread on a copy, so the
`self.bless(|%_, :meta(%_))` shape is served before the argument stretch too.
The registry generation and the class's constructor plan pin the memo.

The ctor and bless lanes now take their nomination before the construction
runs: a TWEAK that calls a method cleared the candidate, so such a class never
entered either lane.

On `bench-ctor` (callgrind, per construction): 221.2k → 210.3k instructions;
on a plain `method new(*%_) { self.bless(|%_) }` class: 83.8k → 74.0k (#19399).
