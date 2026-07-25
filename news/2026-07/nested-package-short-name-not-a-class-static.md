# A doubly-nested package's short name is no longer mistaken for a class-body static

Found 2026-07-25 in File::Ignore (`TODO_dist` T-057), the last failing file of
that dist once the `<( … )>` and per-match-`:my` fixes landed. It reduced to a
snippet with no module and no I/O:

```raku
class O {
    class I { class C { } }                 # <-- two levels of nesting
    method w() {
        sub r() { take 7 }
        gather r()
    }
}
say O.new.w.List;   # raku: (7)   mutsu: ()
```

Three things each made it go away, which bracketed the cause tightly: **one**
level of nesting worked; calling the nested sub **outside** `gather` worked; and
a **top-level** sub called inside `gather` worked even with a doubly-nested class
in scope.

## Root cause

When a class is declared inside another class, `exec_class_decl` binds its
**short** name into the current env so it resolves within the enclosing class
body (`Frog` inside `Forest`). A class body deliberately does *not* restore its
env on the success path — that is how class-body `my` statics survive to be
recorded. Those two facts compose badly at two levels of nesting: registering
`O::I::C` binds a bare `C` into `I`'s body env, and since `I`'s body keeps its
env, `C` was still there when `O`'s body finished.

`register_class_decl` then computes a class's body statics as "env names that are
new since the body started", so it recorded `C` as a class-body `my` of `O` and
stored it in `class_body_static_names["O"]`. Method dispatch consults exactly
that map: a class with body statics gets `current_package` switched to the class
so a method read resolves them via `package_scope_lexical`. So every method of
`O` ran under `current_package = "O"` instead of `GLOBAL`, and the method-body
`sub r` registered as `O::r`. A `gather` body is forced *after* the method
returns, with `current_package` back at `GLOBAL`, so its by-name call looked for
`GLOBAL::r` and failed. In the reduced form that surfaced as a silent empty
`gather`; in the dist, where the sub recurses, it surfaced as
`Unknown function: recurse`.

The single-nesting case worked only by accident — nothing leaked, no statics were
recorded, `current_package` stayed `GLOBAL`, and registration and lookup agreed.

## Fix

A package type object left in the body env is not a class-body static unless the
body really declared it, so `register_class_decl` now skips a candidate whose
value is a `Package` and whose name is not in `declared_statics` (the set of
names the body actually `my`/`state`-declared). A genuine `my $x = SomeType`
static is still recorded, because its name is in that set.

`File::Ignore` now passes all seven files, 103/103, closing that dist.

Pin: `t/nested-package-not-a-class-static.t`, which covers the bare and block
`gather` forms, the recursive dist shape, a doubly-nested `role`, and both
directions of the static-detection guard.
