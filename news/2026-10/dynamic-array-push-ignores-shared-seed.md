# A push to `@*x` extends the declared array after a thread has started

```raku
sub req() {
    my @*BF;
    await start { 1 };
    { @*BF.push(1) }();
    @*BF.elems
}
say req();   # 1
```

Starting a thread copies the visible variables into a name-keyed store
that threads share. A dynamic array that was visible at that moment kept
an entry there. A later push to `@*BF`, from a block or a regex assertion,
went into that entry and rebound the running frame's `@*BF` to it. The
array the frame declared stayed empty. The push used to reach the
declaring frame only because a closure's return copied its dynamics back.
A dynamic belongs to its frame, so the shared store no longer stands in
for one, and that copy-back is gone (#13732, ADR-12529 §3, slice 44). The
bundled Cro::HTTP router, which collects bind failures in `@*BIND-FAILS`
from a regex assertion, answers 401 rather than 404 without that
copy-back again.
