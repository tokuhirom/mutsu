# A routine's match reset no longer deletes its caller's named captures

`$/` and its capture views (`$0`, `$<name>`) are implicitly `my`-declared per
routine, so a sub that matches internally must not disturb its caller's match.
mutsu enforced that with the scoped env overlay the zero-argument compiled fast
call installs: the callee's `reset_capture_env_vars` REMOVES inherited
`$<name>` keys, and inside an overlay that removal is a callee-local tombstone
that is dropped on return instead of reaching the caller.

That overlay was gated on `!opcode::reflective_name_access_possible()`, a
**process-global, monotonic** flag latched at compile time by any `EVAL`,
`CALLER::`, symbolic deref or pseudo-stash op anywhere in the program. One
`EVAL` therefore turned the boundary off for *every* zero-local routine in the
program, and such a routine's capture reset then ran directly against the
caller's env — deleting the caller's `$<first>` outright:

```raku
my $unused = EVAL '1';                      # latches the flag
sub inner-match() { "zz" ~~ /(z)/; 1 }
"abc" ~~ /$<first>=(b)(c)/;
inner-match();
say ~$<first>;                              # was '', rakudo says 'b'
```

Without the `EVAL` line the same program was correct, which is what kept this
hidden: the failure needs a reflective op somewhere else entirely. It surfaced
through the vendored upstream `Test` module (`todo/deep/vendor-real-test-module.md`),
whose `throws-like`/`eval-dies-ok` EVAL a string — so *every* file that loads
it latched the flag and lost the boundary, and
`t/match-vars-are-routine-scoped.t` failed under `MUTSU_REAL_TEST=1` while
passing under the native provider.

The gate is gone: the overlay is installed whenever the body needs a boundary
(it has locals, or it is a routine that writes env), regardless of the flag.
The invariant the gate stood in for — that no full-view iteration consumer is
starved of parent lexicals — is enforced structurally instead, by flattening a
scoped env in everything that captures or clones it across a boundary
(`docs/vm-dual-store.md` Slice 6). The positional-light path has always
installed the same overlay without consulting the flag, so the two zero-arg and
positional call paths now agree.

Pinned by `t/match-vars-scoped-under-eval.t`, which reproduces the bug with a
bare `EVAL '1'` and no Test module at all.
