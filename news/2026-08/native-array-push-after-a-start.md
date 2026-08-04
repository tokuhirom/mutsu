# A native array kept truncating after a thread was spawned

`my uint32 @W` truncates every stored value to 32 bits. It stopped doing so —
process-wide, permanently — as soon as anything anywhere ran a `start`.

The VM's array-push op has a separate branch for `shared_vars_active`, the flag
that latches on at the first thread spawn so that concurrent `@a.push` calls
serialize through the atomic shared store instead of clobbering each other. That
branch pushed the value straight into the container, skipping the two steps the
single-threaded path applies first: the declared **element type check** and the
**native-width wrap**. A second `ContainerRef` branch further down skipped them
for the same reason.

The failure is silent and cross-module, which is what makes it worth writing
down. In the bundled `Digest` distribution, `Digest::RIPEMD`'s `rmd160` runs the
two halves of each compression round in `start` blocks. After calling it, an
unrelated `Digest::SHA1::sha1` returned a **wrong digest** — `sha1("abc")` came
out `360737f7…` instead of `a9993e36…` — because `sha1-block` builds its message
schedule with

```raku
my uint32 @W = $M;
@W.push: S(1, [+^] @W[$_ X- <3 8 14 16>]) for 16..79;
```

and the schedule words silently grew past 32 bits (`@W[19]` became
`0x1_8589_8e01`). `sha1("")` still looked right — an empty message has no bytes
to overflow with — so the corruption depended on the input, which is the worst
shape a hashing bug can have.

Both checks now live in helpers (`check_push_element_type`,
`wrap_native_int_push_value`) that every push path calls, so the element type
governs the push regardless of the target's shape or the process's threading
history.

Pin: `t/native-array-push-after-start.t` — `uint32` truncation before and after
a `start`, `int8` signed wrapping, multi-value pushes, and the type check still
rejecting a bad element. Every assertion passes under rakudo too.
