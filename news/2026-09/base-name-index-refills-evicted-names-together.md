# The base-name dispatch index refills evicted names in one pass

`fn_keys_by_base` groups the registry's function keys by base name, and every
name-keyed candidate gather reads from it. Since #8314 a registration evicts only its
own base name. But the refill was still per name: the next lookup of an evicted, or
never-seen, name scanned the whole functions map for that one name.
[#9665](https://github.com/tokuhirom/mutsu/issues/9665) hit this through a stash
`&` binding of a multi (`Foo::{'&f'} = &f`, `OUR::{'&f'} := &f`).
`register_package_code_alias` re-aliased the multi's candidates by walking every
registered function twice: once in `resolve_all_multi_candidates` and once to find
the candidates' keys. So a re-export loop such as

```raku
BEGIN for <&a &b &c> { EXPORT::DEFAULT::{$_} = ::($_) }
```

cost O(n·r) for n names and r registered functions.

## What changed

- The re-alias reads the multi's family from the base-name index
  (`resolve_all_multi_candidates_indexed`, plus a new `multi_family_aliases` helper
  that `export_implicit_stash_sub` shares). It evicts only the keys it installed
  instead of clearing every dispatch cache.
- The index can now be **complete**, as described in the new
  `runtime::fn_keys_index` module. The second miss after a clear builds an entry for
  every base name in one pass. From then on, a name nobody evicted is answered
  without a scan, including names that have no keys at all. An eviction marks its
  base name dirty, and a miss on a dirty name refills every dirty name in one pass.
  The eviction contract callers rely on ("any key names its whole base name") is
  unchanged.
- The candidate gather's dedup now keeps the smallest registry key per candidate as
  the specificity sort's last tie-break, not the first key seen. This makes the
  order independent of how the keys were gathered, which matters now that index
  entries live longer.

## Measured

`MUTSU_VM_STATS=1` on a program that declares 4000 plain subs and 200 two-candidate
multis, then re-exports the 200 multis in a loop:

| | index scans | keys visited |
| --- | ---: | ---: |
| before, declarations only | 4001 | 7,998,001 |
| after, declarations only | 2 | 1 |
| before, plus the 200 bindings | 4001 (+ 400 uncounted full walks) | 7,998,001 + ~1.8M |
| after, plus the 200 bindings | 3 | 5,001 |

The whole loop now costs one O(r) refill; after that, each binding costs its own
family.
