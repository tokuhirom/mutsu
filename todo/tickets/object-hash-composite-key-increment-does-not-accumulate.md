# `%h{$composite}++` on an object hash does not accumulate

Found 2026-09-07 while fixing
`todo/tickets/object-hash-key-keeps-its-itemization.md`
(`news/2026-09/object-hash-key-is-deitemized.md`), as the one row of that
measurement matrix that still diverged. Independent of it: it reproduces with a
plain, non-itemized key, so no itemization is involved and that fix's code path
is never reached.

## Repro

```raku
my %p{Any}; my $p = (3, 4); %p{$p} = 1; %p{$p}++; say %p{$p};
# raku: 2   mutsu: 1
```

A SCALAR key is fine, which is what narrows it:

```raku
my %g{Any}; my $g = 5; %g{$g} = 1; %g{$g}++; say %g{$g};
# both: 2
```

So it is not "object hashes cannot increment" — it is specifically a key whose
`.WHICH` is *derived* (a `List`, and presumably an `Array`/`Hash`/instance)
rather than being the value's own string form.

## Likely shape

Plain assignment, read, `:exists` and `:delete` all agree on one entry for such
a key (pinned by `t/object-hash-key-is-deitemized.t`), so the WHICH encoding
itself is consistent. `++` is a read-modify-write through a *different*
chokepoint — the by-name scalar RMW path (`AtomicCompoundVar` / the `++`/`--`
ops) — which evidently re-encodes the subscript by a different rule and writes
a second entry, or writes back under the un-encoded key. Worth dumping
`%p.keys` and `%p.elems` right after the `++` to see which.

## Check when fixing

`++`, `--`, and a compound assignment (`+=`, `~=`) on an object-hash entry
keyed by a `List`, an `Array`, a `Hash` and a user instance; the same four with
an *itemized* key (`$(3, 4)`), which must keep agreeing with the plain form;
a scalar-keyed object hash, which already works; and
`t/object-hash-key-is-deitemized.t` plus `t/itemized-hash-subscript.t`.
