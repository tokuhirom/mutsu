# `BIND-KEY` works on a hash with a role mixed in

`%h.BIND-KEY(k, v)` (and `self.BIND-KEY(...)` inside a role method) on a hash a
role was mixed into (`my %h does R`) died with `No such method 'BIND-KEY' for
invocant of type 'Hash'`. The `Hash` row now runs for a mixed-in hash, binding
on its shared node (including the caller-variable alias) while the mixin stays.
Found with WriteOnceHash (#15393).
