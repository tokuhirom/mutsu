# A first-set scan reads each character once again

The negated-class grapheme fix (#10875) taught `FirstSet::admits_at` to offer
the engine every position that starts a multi-codepoint grapheme cluster,
because a negated class matches `x` + U+0301 although it rejects `x`. It did so
by calling `grapheme_end` for *every* position the set rejected — the
per-position reject path of every prefiltered scan — and the deterministic
bench series showed it: `bench-regex-long-subject` +20% Ir and
`bench-regex-split-subst` +11% (#11145).

Asked one position at a time, the question needs a look ahead at every rejected
position. Asked as a scan, it does not: only a non-ASCII character can extend
the cluster of the character before it, and `\r\n` is the one cluster made of
ASCII alone. The new `FirstSet::find_admitted` is the loop of the `FirstChar`
and `Inner` scans. It tests each ASCII character against the bitmap with `\r`
added, so a rejected `\r` reaches the exact check. On meeting a non-ASCII
character it looks back once at the ASCII position before it. A rejected ASCII
position therefore costs one load and a bit test again. A unit test pins
`find_admitted` against position-by-position `admits_at` over every subject of
up to four characters, in every window. New TAP cases pin the CRLF cluster
against a negated class and an ASCII base followed by a mark.

Measured with `scripts/bench-det.sh` on the same `main`:

| benchmark | before | after | check deleted (wrong on clusters) |
| --- | ---: | ---: | ---: |
| bench-regex-long-subject | 87.10M | 71.68M | 71.16M |
| bench-regex-split-subst | 478.38M | 437.79M | 437.22M |

The correct check now costs 0.5M Ir and 0.6M Ir on these two benchmarks,
instead of 15.9M and 41.2M.
