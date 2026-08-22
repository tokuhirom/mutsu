# `nextsame` from a user subclass `.new` into a built-in `Version.new` doesn't reach the built-in constructor

Discovered via the doc-diff harness on `raku-doc/doc/Language/functions.rakudoc` (around line
1291).

## Minimal repro

```raku
class LoggedVersion is Version {
    method new(|c) {
        note "New version object created with arguments " ~ c.raku;
        nextsame;
    }
}
say LoggedVersion.new('1.0.2');
```

- `raku`:
  ```
  New version object created with arguments \("1.0.2")
  v1.0.2
  ```
- `mutsu` (`target/debug/mutsu`):
  ```
  New version object created with arguments \("1.0.2")
  LoggedVersion.new
  ```

The `note` line (the user override actually running) matches. The final `say` differs: raku's
`nextsame` correctly dispatches onward to the built-in `Version.new(Str)` constructor, producing
a real `Version` object that stringifies as `v1.0.2`. mutsu's `nextsame` instead appears to fall
through to some generic default `.new`/instance-gist fallback — the printed `LoggedVersion.new`
looks like the default `ClassName.new` gist of a bare, un-parsed `Instance`, not a constructed
`Version`.

## Root cause hypothesis

`nextsame`/`callsame` MRO dispatch presumably walks the user class's method-resolution chain
looking for the next `new` candidate up the inheritance chain. For a class extending a
*built-in* type (`Version`, `Str`, etc.) whose `.new` is implemented natively (not as a
registered user-visible multi/method in the normal dispatch table), the "next method" lookup
likely doesn't know how to reach the native constructor logic, so it silently produces some
generic instance-creation fallback instead. This is the same general shape as other "subclassing
a built-in type loses its native behavior" findings already tracked elsewhere in this backlog
(e.g. `str-subclass-loses-native-stringify.md`), but specific to `.new`/`nextsame` dispatch
rather than stringification.

## Affected files (starting point)

- `src/runtime/methods_object_dispatch_new.rs` — where `.new` dispatch/MRO walking happens for
  a `nextsame`/`callsame` call originating from an overridden `new`
- `src/runtime/dispatch.rs` / `src/runtime/calls.rs` — `nextsame` MRO-walk implementation, to see
  whether it special-cases user methods but has no case for "next candidate is a native builtin
  constructor"
