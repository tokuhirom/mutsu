# An inherited method's recursive nested sub is no longer broken by a subclass's own class-body sub

A method whose body declares a recursive nested `sub` (e.g. Tarjan's
strongly-connected-components algorithm) died `Unknown function: <name>` on
its own self-recursive call -- but only when the method was **inherited**
and invoked through a subclass that declares its own, entirely unrelated
class-body `sub`.

Found via the `Graph` ecosystem distribution's `t/22-leaper-graph.rakutest`:
`Graph::Leaper is Graph`, where `Graph` composes `Graph::Componentish`'s
private `!tarjan-scc` (a recursive nested `sub strongconnect`), and
`Graph::Leaper`'s own class body declares unrelated helper subs
(`check-moves`, `process-moves`). The minimal repro reduces all the way down
to plain class inheritance, no role required:

```raku
class Base {
    method run() {
        my @result;
        sub strongconnect($v) {
            @result.push($v);
            strongconnect($v - 1) if $v > 0;
        }
        strongconnect(2);
        return @result;
    }
}
class Sub is Base {
    sub check-moves($moves) { True }   # unrelated, but present
}
Sub.new.run;   # died "Unknown function: strongconnect"
```

Root cause: method dispatch (`call_compiled_method` and its "fast path"
twin, both in `src/vm/vm_method_dispatch.rs`) anchors `current_package` to
whichever class has class-scoped subs, so a method can resolve them by bare
name (the fix for #8883). That decision was keyed on `receiver_class_name`
-- self's DYNAMIC class -- rather than `owner_class`, the class/role that
LEXICALLY declares the method body. Invoking an inherited method through a
subclass with its own unrelated sub flipped `current_package` to the
subclass for the whole call, and the nested sub's self-recursive call,
resolved under that now-mismatched package, could no longer find itself.

Fixed by keying both call sites on `owner_class` instead. `owner_class`
already equals `receiver_class_name` whenever the method is not inherited,
so this is a no-op for the common case and for
`t/oo/method/class-body-use-import-visible-in-method.t` (#8883's own
regression test, which involves no inheritance either). Pinned by
`t/oo/method/inherited-method-recursive-nested-sub-unaffected-by-subclass-subs.t`.

Closes #9008.
