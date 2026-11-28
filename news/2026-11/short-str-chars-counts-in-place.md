# `.chars` counts a short string in place

`Str.chars` built a heap-allocated grapheme index for every string below the
index cache's 256-byte threshold just to read its length, and dropped it again:
an allocation, a free and the index bookkeeping on each call. A short string is
now counted directly — its byte length when it is flat ASCII, else its
grapheme count — and only a string past the threshold uses the cached index.

`$c += $s.chars` in a loop: 3,210 → 3,047 instructions per iteration
(profiling build, callgrind). A regression test for the method-call site's
accessor replay (#19438) — parent, child and overriding classes through one
inherited `$.name` site, mixins, `is rw`, aggregate and Proxy attributes — is
added alongside.
