# Re-`use` of a module in a second block breaks method dispatch on its classes

When the same module is `use`d in two sibling blocks, method calls on a class
the module declares fail in the second block with
`X::Method::NotFound: Unknown method new on ScanCacheHelper::ScanCacheThing`
(note the *qualified* name in the message). Smartmatch type checks
(`ScanCacheThing ~~ Mu`, enum `~~` checks) and exported sub calls still work —
only method dispatch on the class is lost, which suggests the second import
resolves the bareword to a bare package value instead of the registered class.

Minimal repro (module: any `class ... is export`/plain class in a
`unit module`, e.g. `t/lib/ScanCacheHelper.rakumod`):

```raku
use lib 't/lib';
{
    use ScanCacheHelper;
    say ScanCacheThing.new.label;   # works: "thing"
}
{
    use ScanCacheHelper;
    say ScanCacheThing.new.label;   # X::Method::NotFound: ... new on
                                    # ScanCacheHelper::ScanCacheThing
}
```

`raku` runs both blocks fine. Verified pre-existing before the module export
scan cache change (2026-07-30, reproduces on an unmodified main build): the
suspect is the runtime's already-loaded re-`use` path
(`runtime_module.rs` `use_module_with_tags_inner`, the `loaded_modules`
early-return branch — `reinstate_module_package_globals` /
`package_stash_hidden` handling), not the parser.

Found while pinning the export scan cache
(`t/module-export-scan-cache.t`, which sidesteps `.new` in its second block
because of this bug).
