# `is default(0 but role :: {...})` on a typed Hash drops the role mixin on the default value

Discovered via the doc-diff harness on `raku-doc/doc/Language/objects.rakudoc` (around line
1526).

## Repro

```
my %seen of Int is default(0 but role :: { method Str() {"NULL"} });
say %seen<not-there>;
```

- raku: `NULL`
- mutsu: `0` — the role mixin on the default value is not applied when the default is returned
  for a missing key

## Root cause guess

The Hash's `is default(...)` value is presumably stored/cloned in a way that only keeps the
"plain" `Int` payload and drops the mixed-in anonymous role, so every miss-lookup returns the
un-mixed base value instead of the `but`-mixed one.

**Possibly the same underlying root cause as**
[list-but-role-loses-positional-binding.md](list-but-role-loses-positional-binding.md) and
[role-mixed-value-gist-skipped-in-array.md](role-mixed-value-gist-skipped-in-array.md) — see
that ticket's note on the shared hypothesis. Filed separately because each has a distinct
minimal repro; investigate together and merge into one PR if a single fix site is found.

## Affected files (starting point)

- `src/runtime/class.rs` / wherever `is default(...)` trait values are stored on a typed Hash
- Hash miss-lookup path (returns the default value) — check whether it clones the stored default
  Value directly or reconstructs a fresh one from a type-erased representation

## Suggested next step

Check whether the default value, once stored on the Hash's type metadata, still carries its
mixin flag/role-list when read back via `%h<missing-key>` — compare to a directly-read
`$x = 0 but role :: {...}; say $x;` case (which works — see the `role-mixed-value-gist-skipped-in-array`
ticket's repro) to see where the mixin info is lost specifically on the Hash-default storage
path.
