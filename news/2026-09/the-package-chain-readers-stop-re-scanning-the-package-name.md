# The package-chain readers stop re-scanning the package name

Slice 2 of the `check-name-scans` ratchet
([#8899](https://github.com/tokuhirom/mutsu/issues/8899)). The first slice
built [`src/qualified.rs`](../../src/qualified.rs) and converted the two type
lookups; this one takes the readers that walk a package *chain*.

## What was being redone

`running_package_our_var` — the body of `running_module_bareword`, 1.54% of a
`JSON::Fast` decode with another 0.77% of the whole program in `format!` —
asked five questions per call, every one of them decided by text the source
fixed once:

```rust
if has_double_colon(name) || name.is_empty() { return None; }
for candidate in self.running_package_candidates()... {
    let mut pkg = candidate;
    loop {
        if !pkg.is_empty()
            && pkg != "GLOBAL"
            && !has_routine_scope_marker(pkg)
            && let Some(v) = self.get_our_var(&format!("{pkg}::{name}"))
        { return Some(v.clone()); }
        match pkg.rsplit_once("::") { Some((parent, _)) => pkg = parent, None => break }
    }
}
```

Six sites in `vm_env_helpers.rs` had the same shape one layer down —
`package_chain_var_fallback`, `package_scope_lexical` (166,760 calls in a
ten-decode profile), `auto_qualified_bare_env_read` and the
`unit_lexical_slot` pair. Those already read the interned mirror rather than
cloning the package name, and then took a `&str` off it and string-compared
`"GLOBAL"` and byte-scanned for `"::&"` anyway.

## The new classification

`is_routine_scoped_package` joins `is_qualified`: a routine-scope mangled name
(`Pkg::&sub/arity`, used for nested subs) is not a package and must not be
walked as one, which is why every chain-walking site asks it right after
asking whether the package is `GLOBAL`.

The two share one flag-table entry. The table is indexed by symbol id and
classifies both on first ask, so the second question about the same symbol is
free — which matters precisely because these sites ask both.

## Measured

Paired A/B re-baselined from the current `main` (which already carries slice
1), both sides a fresh `--profile profiling` build, one `from-json` ×10 each:

| | before | after |
|---|---:|---:|
| total instructions | 27,786,351,795 | 27,501,781,324 (**−1.02%**) |
| `StrSearcher::new` (the `"::"` searches) | 224,529,610 | 158,625,100 (−29.4%) |
| `alloc::fmt::format::format_inner` | 130,139,046 | 91,951,758 (−29.3%) |
| `core::fmt::write` | 116,191,806 | 84,471,720 (−27.3%) |
| `TwoWaySearcher::next_back` (the `rsplit`s) | 83,041,125 | 56,710,131 (−31.7%) |
| `vm_env_helpers` | 339,917,672 | 222,153,103 (−34.6%) |
| `vm_var_get_ops` | 56,402,212 | 31,163,980 (−44.7%) |
| allocations | 17,421,035 | 17,113,286 (−1.8%) |

The memo is not free and the A/B shows its price: `qualified::flags::of`
arrives at 49.2 M, `HashMap::contains_key` rises 21.2% and `LocalKey::with`
3.8%. −1.02% is the net.

One row wants explaining rather than claiming: `PackageKeyed::contains` reads
210,771,106 → 0. That is an inlining artifact, not a code path that stopped
running — `contains_key` rose by 181 M and `hash_one` by 28 M in the same run,
which is where those instructions went. Its only callers are in
`type_registry.rs`, which this slice does not touch.

Under the ~2% threshold where this box's wall clock can see anything, so no
wall-clock claim is made.

Ratchet: qualify 174 → 173, global-cmp 99 → 92, scan 348 → 346.
