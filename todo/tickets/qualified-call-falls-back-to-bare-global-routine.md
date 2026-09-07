# A package-qualified call falls back to a bare `GLOBAL::` routine of the same short name

`Pkg::name(...)` resolves to `GLOBAL::name` when `Pkg` declares no such
routine -- even when `Pkg` does not exist at all. Rakudo raises
`Could not find symbol '&name' in 'GLOBAL::Pkg'`.

Minimal repro (no modules involved):

```
$ mutsu -e 'sub zzz($n) { $n + 1 }; say NoSuchPkg::zzz(1);'
2
$ raku  -e 'sub zzz($n) { $n + 1 }; say NoSuchPkg::zzz(1);'
Could not find symbol '&zzz' in 'GLOBAL::NoSuchPkg'
```

## Where it comes from

`Interpreter::resolve_function_with_types`' qualified branch
(`src/runtime/dispatch_resolve.rs`) ends by falling through to the bare-name
machinery once every `Pkg::name`-shaped probe misses. `fn_keys_for_base`
reduces `Pkg::name` to the base name `name`, so the candidate gather that
follows is keyed on the short name and can hand back a routine that lives in a
completely different package. The `qualified_name_hidden_here` gate above it
only hides `my`-scoped package items; it does not assert that the resolved
candidate actually belongs to the named package.

## Why it matters

It is a silent wrong-routine dispatch, not just a missing error: a typo'd or
stale package qualifier calls the caller's own same-named routine and returns a
plausible answer. It also makes an otherwise good assertion untestable --
`t/module-private-sub-does-not-leak.t` wanted to pin "a compunit-private
routine is not reachable as `Mod::name`" and could not, because the script's own
`sub secret-helper` answered the qualified call.

## Shape of the fix

After the qualified probes fail, verify that whatever the fallback resolved
actually has `def.package` equal to (or an ancestor-visible alias of) the
requested package, and otherwise raise the `X::NoSuchSymbol`-shaped error the
stash path already produces (`Could not find symbol '&name' in 'GLOBAL::Pkg'`).
Expect fallout in tests that lean on the sloppy fallback -- the qualified path
is also how a `unit module`'s own `our sub` is reached in several shapes -- so
this needs a full `make test` + `make roast` pass, not a spot check.
