# `$*COLLATION` is now a real process-wide `Collation`, so `.set` persists

```raku
$*COLLATION.set(:quaternary(False), :tertiary(False));
say $*COLLATION.tertiary;   # raku: 0     mutsu: Nil
say 'a' coll 'A';           # raku: Same  mutsu: Less
```

Discovered via the doc-diff harness on `raku-doc/doc/Type/Any.rakudoc` (~line
1420).

## Root cause (not the one the ticket guessed)

The ticket suspected a write-back aliasing problem in `Collation.set`. It is not:
`Value::write_back_sharing` commits into the instance's shared
`Gc<InstanceAttrs>` cell and is perfectly visible to later readers — which is why
the whitelisted `roast/S32-str/Collation.t`, whose every subtest starts with an
explicit `my $*COLLATION = Collation.new`, has been passing all along.

The actual gap was one level up: **mutsu never seeded `$*COLLATION` at all.** A
bare read returned `Nil`, so `.set` was dispatched on `Nil` (returning `Any`) and
every accessor read `Nil`. The `coll`/`unicmp` operators appeared unaffected only
because `get_collation_settings` silently falls back to
`CollationSettings::default()` on a miss — so they always used the default level
no matter what had been `set`.

## Fix

`$*COLLATION` joins `$*DISTRO`/`$*PERL`/`$*RAKU`/`$*VM`/`$*KERNEL` in
`Interpreter::lazy_magic_dynamic_var` (`src/runtime/io_env.rs`), materialized on
first read from a `OnceLock`-cached `Collation` instance with every level enabled
(`collation-level => 85`). This models rakudo faithfully: `$*COLLATION` lives in
`PROCESS::` as one mutable object, `Collation.set` mutates *that* object and
returns it, and clones of the cached `Value` share the same
`Gc<InstanceAttrs>` cell — so a `.set` made anywhere is observed everywhere
afterwards, including inside a called sub, inside a `for` body, and by `coll`. A
`my $*COLLATION = Collation.new` still shadows it lexically. Building it lazily
keeps the cost off programs that never touch collation.

A `raku` arm was added alongside the existing `gist` arm in
`dispatch_collation_method`: rakudo's `Collation` holds a single
`$!collation-level` attribute and derives the four levels from its bits, so its
`.raku` is `Collation.new(collation-level => N)`. mutsu stores the four levels
separately, so the generic instance `.raku` rendered `Collation.new` with no
attributes at all; the arm re-encodes the level exactly as `gist` already did.

Verified against `raku` v2026.06: initial `.raku`/`.gist`/`.primary`/`.Bool`,
`.set` persistence into a sub and a `for` body, `coll` observing the change,
lexical shadowing by `my $*COLLATION`, `.clone` independence, and
`Collation.new.set(:!tertiary).raku` all agree. `roast/S32-str/Collation.t`
still passes. Pinned by `t/lexical-decl-and-autoviv.t`.
