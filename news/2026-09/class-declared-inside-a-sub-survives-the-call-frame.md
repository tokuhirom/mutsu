# A class/role/grammar/enum declared inside a sub is now findable via `::()` after it returns

`class Foo { ... }` (and the equivalent `role`/`grammar`/`enum` forms), when declared and run
inside a named or anonymous sub's body, installs into the enclosing *package* in real Raku,
regardless of how deeply the declaration is nested inside call frames. mutsu instead only wrote the
bareword binding into the currently executing lexical env tier -- exactly what a sub call frame
discards on return -- so `::('Foo')` (indirect symbolic package lookup) came back a `Failure` once
the declaring sub had returned, even though the value it produced (`Foo.new`) still reported the
right `.WHAT`/`.^name` (that identity comes from the registry, not the env).

This mattered beyond the direct symptom: `Test.rakumod`'s real `isa-ok` calls
`nqp::istype($var, $type.WHAT)` for a non-`Str` expected type, so a `Failure` from `::()` fed a
silent `False` into the type check instead of throwing -- the bug behind
[#8683](https://github.com/tokuhirom/mutsu/issues/8683), found while working the
`Object::Permission` distribution in the `ecosystem-dist-roulette` sweep.

## Fix

`resolve_indirect_type_name` now falls back to the class/role/enum *registry* (which persists for
the whole process, unlike the frame-scoped env) when a bareword lookup misses the env, so it survives
a returned call frame. A `my`-scoped declaration is excluded correctly: a namespaced one is already
guarded by the existing `is_my_scoped_type_name` check, a bare one registers under a call-frame-
mangled storage key so `has_class`/`is_role` never see its unmangled name, and a `my enum` (which
does not mangle its storage name) is now explicitly marked `my`-scoped so it stays excluded too.

A second distinction had to be preserved: a class loaded by a nested runtime `require` (not a plain
`class` statement) installs into the *current lexical scope* of the `require` call in real Raku, not
the enclosing package -- so it must NOT survive the call frame that ran the `require`, unlike an
ordinary declaration. `roast/S11-modules/require.t`'s `GlobalOuter.load` (which requires
`GlobalInner` and expects `::('GlobalInner')` to succeed while `.load` is running but fail again
once it returns) pins this. A new `require_loaded_type_names` set records every class/role/enum a
`require_load_from_file` call newly registers, and the registry-backed fallback refuses to trust
those names -- they keep relying purely on the ordinary frame-scoped `env` entry, which already gives
them the correct frame-lifetime-bound visibility.

Regression test: `t/oo/class/class-decl-in-sub-symbolic-lookup.t`.
