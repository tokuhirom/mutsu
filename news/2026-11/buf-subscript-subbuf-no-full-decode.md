# `$buf[$i]` and `subbuf` no longer decode the whole buffer

Redis's `t/03-large-replies` hung on mutsu (#18643). An `mget` of 400 keys
returns a reply of about 200 KB, and Redis's reply parser scans it with
`$input[$i]` and peels each element off with `subbuf`. Both operations decoded
the *whole* buffer into boxed `Value`s on every call, so one subscript cost
O(buffer). Parsing the reply took 1.6 s on mutsu (raku: 0.05 s), which is past
the test server's `timeout 1`. Redis then closed the idle connection, and the
next command looped forever, reading empty `recv` results at EOF.

The subscript (`Blob`/`Buf` `AT-POS`) now decodes only the element it reads,
through the existing `buf_elem_at`. `subbuf` copies the requested run as
storage bytes through a new `value_buf::buf_slice`, so it costs O(taken) rather
than O(buffer). A re-tagged node, whose layout differs from its class, keeps
the per-element path. The reply now parses in 0.05 s, and all 17 Redis test
files pass.
