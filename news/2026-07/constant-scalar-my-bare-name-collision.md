# A module-level `my $NAME` no longer collides with a same-named `constant`

Fixes PLAN.md §1 B4 bug #3 — the HTTP::UserAgent battery blocker where a request's
line-ending turned into a `Buf` and threw "Cannot use a Buf as a string".

## The shape

mutsu strips the `$` sigil from scalar names in the AST, so a scalar `my $CRLF`
and a sigilless `constant CRLF` share the bare key `CRLF`. The bug triggered when
**both** a parent and a child class (in separate modules) declared `my $CRLF` and
a same-named `constant CRLF` existed in a third module — exactly
`HTTP::Message`/`HTTP::Request` (both `my $CRLF = "\r\n"`) plus `HTTP::UserAgent`'s
`constant CRLF = Buf.new(13, 10)`.

Minimal repro (`t/constant-scalar-my-bare-name-collision.t` with fixtures under
`t/lib/Crlf*.rakumod`):

```
# CrlfBase.rakumod     unit class CrlfBase;      my $CRLF = "base-crlf";    method base-crlf() { $CRLF }
# CrlfDerived.rakumod  use CrlfBase; unit class CrlfDerived is CrlfBase; my $CRLF = "derived-crlf"; method derived-crlf() { $CRLF }
# CrlfConst.rakumod    unit class CrlfConst; use CrlfDerived; constant CRLF = "const-crlf";
# run                  use CrlfConst; use CrlfDerived; say CrlfDerived.derived-crlf;   # was: const-crlf   now: derived-crlf
```

## Root cause

On the class-registration success path a class body's top-level `my` statics are
intentionally left in the persistent mainline env (so a top-level `class`/`use`
keeps them reachable), and are *also* mirrored into `package_lexicals[Class]` /
`class_body_static_names[Class]` so a method can resolve them after the body env
is gone. A method read of such a static goes through `package_scope_lexical`,
which is gated on the class having recorded statics.

`register_class_decl` recognized a class-body static by it being **new** in `env`
(absent from the pre-body `saved_env`). But the parent module's `my $CRLF` had
already leaked into the persistent mainline env, so by the time the child class
registered, `saved_env` already carried `CRLF`. The child's own `my $CRLF` was
therefore *not* recorded as a class-body static, `class_has_package_lexicals`
returned false for it, and its methods fell back to the bare-name global — which
the last writer (the `constant CRLF` `Buf`) had clobbered.

## Fix

A name a class body **explicitly declares** with `my`/`state` at top level is a
class-body static regardless of any pre-existing outer/leaked binding of the same
name. `register_class_decl` now collects the set of top-level `Stmt::VarDecl`
names (non-`our`, non-dynamic) from the class body and treats a name as a static
when it is either new in `env` **or** in that declared set. The leaked outer
binding no longer suppresses the child's own static, so `package_scope_lexical`
resolves each class's `$CRLF` to its own value. Names the body merely *reads* from
an enclosing scope are unaffected (they are not in the declared set), so a class
that reads an outer lexical still sees the live outer value.

With this, the real `HTTP::Request.new(GET => $url).Str` renders `\r\n`
line-endings as a `Str` again instead of a `Buf`.
