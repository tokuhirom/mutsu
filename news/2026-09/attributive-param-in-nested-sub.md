# An attributive parameter now works in a `sub` nested in a method

Raku's rule for `$!x`/`$.x` parameters is lexical: a plain `sub` nested
directly in a method or submethod body closes over the same `self` the
method itself has, so it may declare an attributive parameter exactly like
the method can. mutsu rejected this unconditionally at parse time, and —
even once that check was relaxed — silently dropped the bound value instead
of writing it to the object.

## Gap 1 — parse-time rejection was per-routine, not lexical

`reject_attr_params_in_sub` used to reject `$!x`/`$.x` parameters in every
`sub` signature. The rule is now scope-aware: `LexicalScope` tracks a
`self_available` flag that a method/submethod body's scope sets
(`mark_current_scope_self_available`, via the new `stmt::method_block`) and
that a nested class/role/grammar/package body's scope clears
(`clear_current_scope_self_available`, via the new `stmt::package_body_block`
and `parse_block_body_no_self`). Unlike the routine-body flag, this one IS
inherited by every scope nested inside it (`push_scope`'s clone), so a `sub`
declared anywhere in a method's body — directly or nested in further blocks —
sees `self` as available too, and a class/role/grammar/package body
interposed anywhere in between resets it, matching rakudo exactly:

```
sub s($!t) { }                                          # rejected (top level)
class C { has $!t; sub s($!t) { } }                      # rejected (class body)
class C { has $!t; method m { sub s($!t) { } } }         # accepted
class C { has $!t; method m { class D { sub s($!t) { } } } }  # rejected (nested class)
class C { has $!t; method m { role R { sub s($!t) { } } } }   # rejected (nested role)
```

## Gap 2 — the binder wrote a throwaway local, not the attribute

An attributive parameter *means* "bind this parameter to `self.attr`". The
binder only ever wrote the value into the callee frame's env/locals keyed by
the twigil name (`!t`); for a method this happened to look right because a
method frame IS the attribute-cell mirror source, but a nested named `sub`
has its own frame, so the write went nowhere anyone read.

The fix reuses the same post-bind mirror step a method's own attributive
parameter already gets (`mirror_attributive_params_to_cell`, generalized to
take `param_defs` directly instead of a `MethodDef`) and calls it from a
plain sub's call path too (`call_compiled_function_named_inner`). Two
positional-parameter fast-dispatch caches (`is_light_call_eligible`,
`is_positional_light_call_eligible`) bypassed that call entirely, so both
now exclude a signature carrying an attributive parameter, forcing such a
call through the path that performs the mirror.

```raku
class C {
  has $!t = "orig";
  method m {
    sub s($!t) { say "inside: $!t" }
    s("new");
    say "after: $!t";
  }
}
C.new.m;
# was: inside: orig / after: orig
# now: inside: new  / after: new
```

Refs #8452.
