# Text::CSV runtime sweep round 6: rw-multi cache soundness, inherited IO::Handle chomp, $self assignment, QuantHash slice adverbs

Four general interpreter fixes found by chasing Text::CSV's `85_util.t`
(github.com/Tux/CSV) from aborting at test 5 to fully passing (110/110),
bringing the suite to 29/33 fully-green files.

1. **The type-keyed multi resolution cache must exclude `is rw`
   candidates.** `multi_resolve_cache` (and its sub twin) caches the winner
   for a multi whose dispatch is "purely type+arity based", but an `is rw`
   candidate matches only a writable-lvalue argument — a call-site property
   invisible to the type key. Text::IO::String's `multi method new (Str $str!
   is rw, ...)` / `(Str $str!, ...)` pair meant whichever call ran first froze
   the winner: `new($data)` then `new($str.Str)` threw X::Parameter::RW, the
   reverse order routed variables to the copy candidate. Both cacheability
   scans now treat an `is rw` param as value-dependent. Pin:
   `t/rw-multi-dispatch-cache.t`.

2. **A pure-Raku `is IO::Handle` subclass inherits the `$.chomp is rw`
   accessor.** mutsu's IO::Handle is native (exact-class checks plus a handle
   id); a blessed subclass instance has neither, so `.chomp` fell through to
   the native *Str* chomp (returning `"Text::IO::String()"` — the stringified
   invocant) and assignment died. The native fast path now routes every
   instance to the slow path, which answers the read (stored attribute, else
   the IO::Handle default True) and the lvalue assign (stores the attribute)
   for any class whose MRO includes IO::Handle without a user-declared
   `chomp`. Pin: `t/io-handle-subclass-chomp.t`.

3. **`my $self` is assignable outside method bodies.** Scalars are stored
   sigil-less, so the compile-time "`self` is immutable" check (emitting
   `AssignReadOnly` for the invocant keyword) also fired for a user variable
   named `$self` — `$self = $csv.header($fh)` threw X::Assignment::RO before
   header even ran, and the test's CATCH captured the wrong exception. The
   check is now gated on `lexically_in_method`, keeping the roast pin
   (`S12-class/basic.t`: `method f { self = 5 }` throws). Pin:
   `t/self-var-assign.t`.

4. **Subscript adverbs work on QuantHash slices.** `$bag{@keys}:kv` /
   `:v` / `:k` / `:p` returned empty: the adverb builtin had Array and Hash
   arms only, and a Set/Bag/Mix target fell into the
   "associative-on-non-associative" everything-is-missing path. The target is
   now projected through `.hash` (which decodes the internal WHICH-encoded
   keys — the raw-internal-map projection kept `Str|,`-style keys and matched
   nothing) and the Hash arm owns the logic. Text::CSV's header separator
   detection is exactly `$hdr.comb.Bag{$sep-set.list}:kv`, so `header` never
   detected `;`/tab separators and never raised the 1011 sep-conflict error.
   Pin: `t/quanthash-slice-adverbs.t`.

Suite status: `85_util.t` joins the green set (29/33). Remaining: 90_csv
(csv() header semantics), 91_csv_cb, 92_csv_encoding, and 99_meta (needs
external Test::META).
