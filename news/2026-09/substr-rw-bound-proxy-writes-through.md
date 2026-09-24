# A bound `substr-rw` Proxy writes through to its string

`my $r := $s.substr-rw(1,1); $r = "Y"` died with "Cannot assign to an
immutable value", and the sub form `my $r := substr-rw($s,1,1); $r = "Y"` set
`$s` to `"(Yny)"` (issue #9200). Both now splice into `$s` (`pYab`), as in
rakudo.

The sub form already returned a Proxy (`make_substr_rw_proxy`) whose FETCH and
STORE closures read and write `$s` by name. But the compiler's free-variable
analysis cannot see those runtime-built closures, so it treated `$s` as a
slot-only local and skipped mirroring it into the env. A FETCH happened to
force a sync, but a STORE with no FETCH before it read an undefined `$s`,
spliced into `"(Any)"`, and wrote `"(Yny)"` back. The compiler now registers
the first argument of `substr-rw`/`subbuf-rw`, and the receiver of
`$s.substr-rw(...)`, as an rw-arg sink (`note_atomic_env_sync_target`, the
mechanism `cas` already uses), which keeps it env-synced.

The method form never built a Proxy at all: `.substr-rw` outside an assignment
just returned the substring. `CallMethodMut` now returns the same Proxy for a
`Str` receiver held in a plain scalar; a user class's own `substr-rw` method is
unaffected.

The `subbuf-rw` counterparts (the plain routine is not even declared) and
Proxy length tracking across repeated stores are filed as #9216.

Pinned by `t/regex/subst/substr-rw-bound-proxy.t`.
