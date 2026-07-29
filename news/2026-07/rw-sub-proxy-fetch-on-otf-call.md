# An `is rw` sub's `Proxy` result is FETCHed on the on-the-fly-compiled call branch too

Calling an `is rw` sub whose result is a `Proxy` must FETCH it in value context —
that is how `cglobal` works, since NativeCall's `cglobal` is a Raku sub returning
a `Proxy` whose `FETCH` reads the C global. Every branch of
`dispatch_func_call_inner` did that except the four that run the callee as
**on-the-fly-compiled bytecode**, which returned the `Proxy` raw.

That branch is not exotic: it is what a file-scope sub takes when it is called
from a *method* body, because the method's compiled-function table does not carry
it. So the same call behaved differently depending on where it appeared:

```raku
sub s()  { my $x = (try cglobal('libnope.so.0', 'sym', Pointer)); $x.^name }
class C { method m() { my $x = (try cglobal('libnope.so.0', 'sym', Pointer)); $x.^name } }
s();      # Any  — the FETCH ran inside the try and its failure was caught
C.m;      # died — the Proxy was stored raw, so the FETCH ran at `.^name`,
          #        outside the try
```

The deferral is what makes it user-visible: the `Proxy` sits in the variable
until the next read, so the FETCH — and any exception it raises — lands past
whatever `try` was wrapping the call. `NativeLibs::Loader`'s library probe is
exactly `(try cglobal($lib, $sym, Pointer)) ~~ Pointer`, so a missing library
threw instead of answering `False`.

The four sites now apply `maybe_fetch_rw_proxy` like their sibling branches,
gated on `!def.is_raw` — the same precision the compiled-function branch already
used with `!cf.is_raw`, so an `is raw` routine still hands back its container.
`maybe_fetch_rw_proxy` keeps its own `in_lvalue_assignment` guard, so an lvalue
target is untouched.

Pinned by `t/rw-sub-proxy-fetch-otf.t`, which checks a sub, a method, a private
method, a loop body, and that a throwing FETCH is caught by a surrounding `try`
in both a sub and a method.

This is one of two bugs on `DBIish`'s real mysql path. The other — a `Proxy`
`FETCH` body losing its captured lexical to a same-named *caller* lexical — is
fixed in
[`closure-capture-beats-same-named-caller-lexical.md`](closure-capture-beats-same-named-caller-lexical.md).
