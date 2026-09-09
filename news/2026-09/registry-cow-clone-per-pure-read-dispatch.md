# A pure-read dispatch probe was deep-cloning the whole declaration registry

`Interpreter::class_has_method` and `class_has_user_method` ask a question —
"does this class, or anything in its MRO, define this method?" — and answer it
from the declaration registry without writing anything. Both asked it through
`registry_mut()`.

That is not free. The registry is copy-on-write behind an `Arc`
(`RegistryWriteGuard::deref_mut`): the first *mutable* deref taken while any
other holder shares the `Arc` deep-clones the entire `Registry` — every class,
role, method entry, function key and subset, with all of their `String` keys.
With the Cro/OpenSSL/CBOR stack loaded that is 631 classes, 1757 method entries
and 332 function keys, and the copy costs about 8.1 million instructions.

The share is not exotic. A `supply` block that registers a `whenever` clones the
interpreter for the callback (`clone_for_thread_excluding`), and the clone holds
a registry share for as long as it lives. From that point on, *any* registry
write pays a full copy.

`Cro::HTTP2::GeneralParser` puts both halves in one loop. Each HEADERS frame
registers one `whenever $cancellation` for the new stream, and dispatching the
frame's `self!set-headers` private method reaches `class_has_user_method`. So
every frame shared the `Arc` and then immediately wrote through it: one full
registry deep clone per frame, `registry-cow: clones=` tracking the frame count
exactly (61 clones for 60 frames).

Both probes now consult `Registry::class_has_method_readonly` /
`class_has_user_method_readonly` under a **read** guard first. These are twins of
the existing `Registry::class_mro_readonly`, whose own doc comment already spelled
out the rule these two call sites had missed: resolve every MRO shape that needs
no cache write, and return `None` exactly when the write side would compute *and*
cache, so the caller can fall back. Only that genuine cache-fill case now takes
`registry_mut()`.

On the HTTP/2 request-parser benchmark from
[#7667](https://github.com/tokuhirom/mutsu/issues/7667) — 60 HEADERS frames fed
straight into `Cro::HTTP2::RequestParser` — the registry COW clones drop from 61
to 2, and the per-frame cost falls from 94.3M to 82.4M instructions, **12.6%
fewer**, measured with `callgrind` as the difference between a 60-frame and a
0-frame run so module loading cancels out.

`tests/registry_cow_not_paid_per_supply_registration.rs` pins it by parsing 4 and
then 24 frames and asserting the clone count does not grow with the frame count.
