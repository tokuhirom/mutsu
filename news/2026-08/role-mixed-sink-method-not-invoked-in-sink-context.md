# A role-mixed `.sink` method now runs in sink context

```raku
role R { method sink { say "sunk!" } }
(1) does R;
say "after";
```

printed only `after`. Raku prints `sunk!` first.

## Root cause — two independent gates, both mixin-blind

The ticket's premise ("mutsu never invokes `.sink` at all in this situation") turned out to be
wrong: sink-context dispatch already existed and already worked for a class instance
(`class C { method sink {...} }; C.new;` runs the sink). Two narrower gates were excluding role
mixins.

1. **The runtime gate matched `ValueView::Instance` directly.** `OpCode::SinkPop`'s handler
   (`src/vm/vm_exec_dispatch.rs`) resolved the sink method with
   `if let ValueView::Instance { class_name, .. } = val.view()` and looked `class_name` up in the
   class registry. A `but`/`does`-mixed value is a `ValueView::Mixin`, so it never matched — the
   same silent-`Mixin`-downgrade that was independently breaking string coercion, `:=` binding,
   and `.sort` (see
   [array-but-role-mixin-name-suffix-and-join-str.md](array-but-role-mixin-name-suffix-and-join-str.md)).
2. **The compile-time gate did not recognise a mixin expression as an rvalue.**
   `stmt_value_may_user_sink` (`src/compiler/stmt.rs`) marks a bare statement as
   "may user-sink" only for a method call or a `do { }` block; everything else is excluded because
   mutsu decontainerizes before `SinkPop` and so cannot tell a container return (which raku does
   *not* auto-sink) from a fresh rvalue. A `but`/`does` expression is unambiguously fresh — it
   constructs a new mixin value — so it is now recognised too.

## Fix

`SinkPop` gained a mixin arm: when the sunk value is a `Mixin` and one of its composed roles
declares `sink` (`Interpreter::mixin_composes_method`, a registry lookup with no dispatch), it is
invoked through `dispatch_mixin_method_call`, wrapped in the same captured-outer writeback
reconcile the class arm already does — that dance is now factored into
`Interpreter::reconcile_locals_from_env` and shared by both arms, rather than duplicated. The
`STORE` exemption (raku sinks a container, not its contents) applies to the wrapped class exactly
as it does to a bare instance. `stmt_value_may_user_sink` accepts `but`/`does` (and looks through
`Expr::Grouped`, so `(1) does R;` counts).

## Known remaining gap (pre-existing, general, already tracked)

The doc's elaborate example still diverges:

```raku
multi increment($b is rw) { ($b + 1) does role { method sink { $b++ } } }
multi increment($b)       { $b + 1 }
my $a = 1; increment($a); say $a;   # raku: 2, mutsu: 1
```

Here the sunk statement is a *sub call*, and mutsu conservatively never auto-sinks a
function-call return — it cannot yet tell an `is rw` (container) return from a plain one, a
limitation recorded as a `TODO` at the `SinkPop` site and as its own row in
`docs/doc-diff-backlog.md` (`Language/contexts.rakudoc:45`). It is not mixin-specific: a plain
`sub f() { C.new }; f();` does not run `C`'s sink either. Fixing it needs first-class container
identity on returns, which is out of this ticket's scope.

Pinned by `t/role-mixin-survival.t`.
