# A wrapped method with `nextcallee` stops recompiling and re-resolving per call

Method::Protected's `is protected` wraps a method in
`my method (|c) { my &original = nextcallee; self.LOCK.protect: { original(self, |c) } }`.
Every call of such a method paid for work that never changed between calls:

- the `nextcallee` callable for the wrapped candidate carried only the method's
  AST, so calling it compiled the body afresh; a dispatcher (proto) wrap parsed
  and compiled its redispatch snippet each time. Both are now compiled once and
  reused (`runtime::value_call_compile_cache`);
- the outermost wrapper ran through the interpreter carrier, which recompiles a
  closure body from its AST, instead of the VM's closure call;
- the candidates after the winner (the deferral tail) were computed on every
  call of a wrapped method and of every resolved multi candidate: an MRO walk
  cloning each candidate `MethodDef` plus a signature match per `multi`. They
  are now built only when `callsame`/`nextsame`/`nextcallee`/`lastcall` first
  reads the frame (`runtime::method_dispatch_lazy_tail`);
- once any method was wrapped, every method call scanned its class for the
  winner's candidate index, wrapped or not;
- `Attribute` and `Lock` had no builtin catalog row, so a method call on one
  recomputed its MRO several times.

Method::Protected's `t/01-basic.rakutest` went from 69 s to 37-44 s on the
profiling build (rakudo: 9 s); the remaining gap is tracked in #12875.
