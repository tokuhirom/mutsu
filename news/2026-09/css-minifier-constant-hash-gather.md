# `constant %h = gather { take ... }` no longer dies on "Odd number of elements"

A `%`-sigiled `constant` initialized from a `gather`/`take` block died with
`X::Hash::Store::OddNumber` ("Odd number of elements found where hash
initializer expected: found 1 element(s)") even when the `gather` yielded a
perfectly even number of pairs. The non-`constant` case (`my %h = gather
{...}`) already worked, because `coerce_object_to_hash` explicitly forces a
`LazyList` before splitting it into pairs; `coerce_constant_hash_value` (the
parallel coercion for `constant %h = ...`) was missing that same
`ValueView::LazyList` arm and fell to its generic scalar fallback, which
treated the whole (unforced) lazy sequence as a single opaque item.

Found via [CSS::Minifier](https://raku.land/zef:sasha/CSS::Minifier) 0.0.14
during an `ecosystem-dist-roulette` draw: `CSS::Minifier::Util`'s
`%HEX-TO-NAME` constant and `CSS::Minifier::Normalizer`'s
`%LONGHAND-TO-SHORTHAND` constant are both built with `gather for ... { take
... }`, so the whole distribution failed to load (`status: blocked_load`).
Fixed in `src/vm/vm_var_assign_coerce.rs` by adding a `ValueView::LazyList`
arm to `coerce_constant_hash_value` that forces the list first, mirroring
the existing `Array`/`Seq`/`Slip` arms. Pinned with
`t/vm/binding/constant-hash-gather.t`.

With the load fixed, CSS::Minifier's ecosystem record moved from
`blocked_load` to `partial` (1/7 baseline files at parity — up from 0/0).
The remaining 6 regressed files (plus one `no_baseline` file blocked by the
sandbox's read-only `/tmp`) all share one further root cause, tracked as
[#8951](https://github.com/tokuhirom/mutsu/issues/8951): a compiled `Regex`
value built with an embedded `@(...)` interpolation
(`rx:i/ << @(%hash.keys) >> /`) re-parses its raw source text — which still
literally contains `@(%hash.keys)` — when later interpolated via `<$re>`
from an unrelated scope, tripping the `contains_dangerous_regex_code`
security check (or silently no-op'ing, depending on the call shape) instead
of reusing the regex's own already-compiled matcher.
