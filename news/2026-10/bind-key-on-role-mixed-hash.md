# `BIND-KEY` on a role-mixed hash aliases the caller's variable

`%h.BIND-KEY(k, $v)` on a hash a role was mixed into (`my %h does R`) now runs
the `Hash` row with the call's receiver and argument sources, so the key is
bound to `$v` itself, as it is for a plain hash: a later `$v = 6` shows through
`%h{k}`. The mixed-in hash was only reached through the source-less path, which
copied the value. Found with WriteOnceHash (#15393).
