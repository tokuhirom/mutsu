# `use` inside a block leaks its imports to the enclosing scope

In raku an import is lexical to the block that asked for it:

```raku
{
    use Test;
    say "inside: ", &ok.defined;   # True
}
say "outside: ", &ok.defined;      # ===SORRY!=== Undeclared routine: ok
```

mutsu answers `True` both times. There *is* a scoping mechanism —
`push_import_scope` / `pop_import_scope` in `runtime/runtime_module.rs` snapshot
and restore the function/class registries around a block — but the imported
names also land in `env` as `&ok`-style entries, and those are not rolled back.

## Where it bites

`roast/S32-list/skip.t` (whitelisted, fails under `MUTSU_REAL_TEST=1`) opens
with a deliberately selective import so that the *core* `skip` routine stays
visible, because `Test` exports one of its own:

```raku
# By default, Test exports a "skip" sub, which interferes with the "skip"
# functionality we want to test here.  Hence the selective import here.
BEGIN my (&plan, &subtest, &is, &is-deeply, &throws-like) = do {
    use Test;
    (&plan, &subtest, &is, &is-deeply, &throws-like)
}
```

With the leak, `Test`'s `skip` is in scope after the `do` block, so
`skip(5, $list)` reaches the TAP directive and the file aborts with

```
skip() was passed a non-integer number of tests.  Did you get the arguments
backwards or use a non-integer number?
```

Under mutsu's native provider the file happens to pass, because the native
`skip` handler disambiguates the list-skip shape
(`skip_call_is_list_skip`) — the leak is invisible until the real module is in
charge and no such shape check exists.

## Minimal repro

```raku
$ cat > /tmp/x.raku <<'EOF'
{ use Test; }
say &ok.defined;
EOF
$ raku /tmp/x.raku    # ===SORRY!=== Undeclared routine: ok
$ mutsu /tmp/x.raku   # True
```

## Why it is not a one-liner

The registry half is already scoped; the `env` half is the whole job, and it has
to distinguish an imported alias (drop on scope exit) from a module's own
qualified definition (keep — `pop_import_scope` already documents that
distinction for the registry, because a sibling block's later `use` re-imports
from those). `use` also drives pragma state (`strict_mode`, `fatal_mode`,
`monkey_typing`, `newline_mode`), which the same snapshot already restores, so
the change is confined to the symbol side.
