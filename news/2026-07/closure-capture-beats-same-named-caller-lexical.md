# A deferred closure keeps its captured lexical against a same-named caller lexical

A closure invoked through the **caller-priority env merge**
(`call_sub_value(…, merge_all = true)`) lost its own captured value for every
lexical the *caller* happened to have declared under the same name. The caller's
variable — an entirely unrelated binding — shadowed the capture, which is
lexical scoping degrading into dynamic scoping.

```raku
our sub mkproxy($libname) is rw {
    Proxy.new(FETCH => -> $ { "saw:$libname" }, STORE => -> $, $ { })
}
sub caller-with() {
    my $libname = 'OUTER';          # same name, unrelated variable
    mkproxy('INNER')
}
say caller-with();                  # was: saw:OUTER (or an uninitialized value)
```

In practice `merge_all` means **`Proxy` `FETCH`/`STORE` bodies**
(`maybe_fetch_rw_proxy` / `auto_fetch_proxy`) plus the other natively-invoked
callbacks (Promise/Supply/reduce/on-switch).

## Why the merge exists, and why a name test cannot fix it

Caller-priority is deliberate: a `FETCH` body must see the *current* value of a
lexical its `STORE` twin mutates (`substr-rw`'s `$str`), and a dynamic variable
must resolve against the live caller chain. Freshness, not identity, is what it
buys. So preferring the capture for every name the body captures is not an
option — it froze `NativeLibs`' `cannon-name` at its first call's `$version`.
"Same name" is simply not "same variable", in either direction.

The sound discriminator already exists, on the other execution path. The VM's
`call_compiled_closure` installs a closure's **authoritative** free variables
with overwrite: the compiler's `authoritative_free_vars` (plus the
runtime-vouched `authoritative_captures`) is exactly the set of plain lexicals
the *creating* frame declares and never mutates after the capture op runs, with
dynamics, the topic, `self` and `__mutsu_*` metadata excluded. For those names
the capture cannot be stale, so the closure genuinely owns them and no caller
lexical may shadow them. The interpreter path simply never applied it.

`call_sub_value` now does, and the `merge_all` caller-priority arm keeps its
freshness role for everything else.

## The merge-back direction had the same defect

Fixing the input merge exposed its mirror image on the way out. The `merge_all`
write-back copied **every** callee env entry whose name the caller also held —
not just the ones the body changed. Before the fix the two values coincided
(caller-priority had just installed the caller's own value), so the clobber was
invisible; afterwards a `FETCH` body's capture flowed straight back out and
overwrote the caller's variable. It now propagates only entries the body
actually changed relative to its body-entry snapshot, which is the same test
`captured_outer_writes` was already applying for the write-through to the
caller's local slot.

## Effect on DBIish

`NativeLibs::Searcher.try-versions('mariadb', 'mysql_init', 0..4)` probes each
candidate with `(try cglobal($cn, $wks, Pointer)) ~~ Pointer`. `cglobal`'s
`FETCH` closure captures `$libname`; `try-versions` has its own
`Str $libname` = `'mariadb'`. Every probe therefore dlopened `mariadb` (which
`resolve_library_candidates` expands to the existing `libmariadb.so`), all five
"succeeded", and the driver then failed to load the `libmariadb.so.0` it had
picked. The merge-back defect compounded it: after the first probe the caller's
`$libname` was itself overwritten with the candidate name.

mutsu now picks `libmariadb.so.3`, matching raku, and `DBDish::mysql` loads and
connects to a live MariaDB. `DBIish.connect` is still blocked one step later by
an unrelated defect recorded in
[`todo/tickets/dbiish-install-driver-loses-native-type.md`](../../todo/tickets/dbiish-install-driver-loses-native-type.md).

Pinned by `t/proxy-fetch-capture-vs-caller-lexical.t`, which covers the shadowing
(caller `my`, caller parameter, method caller, and the write-back across loop
iterations) and, on the other side, that a `Proxy` whose `STORE` mutates its
backing lexical still reads the live value.
