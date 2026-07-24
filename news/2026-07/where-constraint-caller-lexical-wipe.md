# A `where`-constrained module sub no longer wipes every earlier caller lexical

Calling a module sub whose signature carries a `where` constraint reset every
caller lexical declared *before* the call to `Any`:

```raku
# lib/WhereMod.rakumod
unit module WhereMod;
sub w(Int $n, :$c! where .so) is export { $n }
```

```raku
use WhereMod;
my $a = Blob.new(1);
my $b = 99;
my $r = w(1, :c<x>);
say "a={$a.gist} b={$b.gist} r={$r.gist}";
# raku:  a=Blob:0x<01> b=99 r=1
# was:   a=(Any)       b=(Any) r=1
```

Only the earlier lexicals were affected — `$r`, which receives the call's
result, was assigned after the damage and so read correctly. That asymmetry, and
the fact that reading a variable (or running any statement at all) between the
declarations and the call made the problem vanish, was the shape of the bug:
something was rebuilding the caller's local slots from `env`, and `env` only
carried the right value once an unrelated read or write had pushed it there.

## The cause

A `where` clause can legitimately mutate a caller lexical by name
(`where { $t ~= 'a' }`). Such a write lands in `env`, so the owning caller
*slot* has to be refreshed for the caller to observe it. The names to refresh
were determined by snapshotting `env` before the clause and diffing it after.

That diff is not sound. `Env` is a chain of scoped tiers and `Env::iter` walks
only the innermost tier's overlay, while `Env::get` searches the whole chain. A
nested call inside the clause can flatten the parent chain into the current tier
(`Env::scoped_child` does this once the chain passes `MAX_OVERLAY_DEPTH`), and
after that flattening every *inherited* caller lexical appears in `iter` for the
first time — indistinguishable, to the diff, from a name the clause just wrote.
Each was then written back into the caller's slot from its `env` value, which is
the declaration seed `Any`: with the slot authoritative, a plain `my $a = 41`
never pushes `41` to `env`. Hence the wipe, and hence why any statement that did
push a value to `env` hid it.

The precise record already exists: the compiler tracks each block's
`free_var_writes`. The `where` clause now runs through a new
`eval_block_value_recording_writes`, which registers exactly those names for the
caller-slot writeback, and the `env` diff is gone. A clause that mutates a
caller lexical still propagates; a clause that mutates nothing now records
nothing.

## Effect

This was the last gap in the bundled OpenSSL battery.
`OpenSSL::CryptTools`'s `encrypt`/`decrypt` candidates are all
`where`-constrained module subs, so every call wiped the caller's plaintext
`Blob` and the round-trip assertions compared against `Any`.
`t/04-crypt.rakutest` goes from 6/13 to **13/13**, which brings the OpenSSL
suite to **7/7** and the release-time battery gate baseline
(`batteries-whitelist.txt`) to 16/18.

Pin: `t/where-constraint-caller-lexicals.t` (with `t/lib/WhereConstraintMod.rakumod`).
Reproducing needs the module declaration, the `where` constraint, an assigned
result, and a mainline caller frame — the pin documents all four so it is not
accidentally rewritten into a shape that stops testing anything.
