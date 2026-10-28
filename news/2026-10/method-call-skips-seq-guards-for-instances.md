# Method calls on class instances skip the Seq/Slip receiver guards and a per-exit allocation

Three bounded cuts on the plain method-call path, found while profiling `ML::TriesWithFrequencies`
`trie-create` (#15235):

- `CallMethodMut` ran four receiver-shaping steps (`lazy_slip_receiver`, `shaped_iteration_receiver`,
  `take_seq_prefix`, `consume_seq_for_call`) for every receiver. None of them acts on an `Instance`
  (they look for a lazy-tail Slip, a shaped array, or a Seq / Scalar-wrapped Seq), so a user-class
  receiver now skips the chain.
- `frame_container_refs`, run on every method exit, boxed a `dyn Iterator` over the frame's env
  overlay; it now visits the overlay through one closure, with no allocation.
- `Symbol::as_str` read a per-thread `RefCell<Vec<Option<&str>>>` memo. It now reads a lock-free
  global slot table (the same chunked `OnceLock` shape as the symbol flag table), with no
  thread-local access or borrow flag.

Measured with callgrind on a 40,000-iteration `$o.plain(1)` loop, profiling build, the same script on
the `main` binary and on this branch: 669.8M -> 630.3M Ir (-5.9%, about 1,000 Ir per call). The
`trie-create` goal in #15235 is not met by this slice.
