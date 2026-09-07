# `t/io-path-smartmatch-permissions.t` no longer fails when the suite runs as root

`t/io-path-smartmatch-permissions.t` pinned IO::Path smart-matching against a
file test (`$path ~~ :w`) with a single assertion:

```raku
is '/sys'.IO ~~ :w, False, 'IO::Path smart-match uses effective write access';
```

That fixture only holds for an unprivileged user. `/sys` is mode `0555` but is
mounted `rw`, and root bypasses the DAC permission check
(`CAP_DAC_OVERRIDE`), so `access(2)` reports it writable. mutsu answered `True`
— and so does rakudo (`raku -e 'say "/sys".IO ~~ :w'` prints `True` as root),
which is the point: the interpreter was right and the test's assumption was
wrong. Any session running the suite as root (the container/docker development
environment does) saw a deterministic `make test` failure that had nothing to
do with the code under test.

The file now pins the same property — smart-matching an `IO::Path` against
`:e`/`:r`/`:w`/`:x` routes to the corresponding file-test method and actually
consults the permission bits — with uid-independent fixtures on a temporary
file:

- `:e`/`:r`/`:w` are `True` for a freshly created file.
- `:x` is `False` at mode `0644` and `True` at mode `0755`. This is the
  negative assertion that survives root: unlike read and write, `X_OK` is only
  granted to root when at least one of the three execute bits is set, so the
  bits are genuinely observed either way.
- `:w` is `False` for a path that does not exist, for any uid.
- The original "read-only mode is not writable" assertion is kept for
  unprivileged runs and `skip`ped when `+$*USER == 0`, with the reason spelled
  out, so nothing is silently lost when CI runs it as a normal user.

The whole file passes under both `target/debug/mutsu` and `raku` as root and as
an unprivileged user, so it keeps working as an oracle-checked pin.
