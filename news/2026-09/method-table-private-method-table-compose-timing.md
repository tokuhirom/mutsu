# `.^method_table` now hides an auto-accessor mid-compose; `.^private_method_table` implemented

Two MOP introspection gaps filed as #8836 while verifying the real `AttrX::Lazy`
module (a `Math::Matrix` dependency) end to end, on top of the fixes for #8815:

1. **`.^method_table` wrongly included a public attribute's auto-generated
   accessor while a custom `compose` hook was still running.** Raku installs
   that accessor as part of the *native* `Metamodel::ClassHOW.compose`
   implementation, reached via `callsame` — not at attribute declaration
   time. A `compose` override that inspects `type.^method_table` *before*
   calling `callsame` (exactly what `AttrX::Lazy`'s
   `LazyAttributeContainerHOW.compose` does, to detect a name conflict
   before installing a lazy accessor) must see the accessor still absent;
   if it doesn't call `callsame` at all, the accessor never appears, ever.
   mutsu derived `.^method_table` unconditionally from `ClassDef::attributes`,
   so the accessor was always visible, defeating the conflict check.

   Fixed by tracking, per class, whether a custom-HOW `compose` hook is
   currently running (`Interpreter::classes_composing_accessors`) — set
   right before dispatching the hook (both places mutsu currently invokes a
   user `compose`: the attribute-trait-driven call in
   `apply_attribute_traits`, and the `pending_class_compose` queue for an
   EXPORTHOW/DECLARE-installed HOW) and cleared once the call returns.
   `class_method_table`/`collect_class_methods` skip a class's own
   auto-accessors (both instance and class-level attributes) while it is
   composing, keeping `.^method_table` and `.^methods(:local)` in lockstep
   as before.

2. **`.^private_method_table` did not exist at all** ("No such method
   'private_method_table' for invocant of type 'Perl6::Metamodel::ClassHOW'").
   `AttrX::Lazy` calls it to verify a lazy attribute's builder method
   actually exists before installing the accessor. Implemented
   `class_private_method_table`, the mirror of the existing
   `class_method_table`: it walks the same canonical reverse index
   (`Registry::owner_method_names`) but keeps exactly the rows
   `class_method_table` excludes — any method or submethod declared with
   `!` (`is_private`) — and wired it into `dispatch_classhow_method` plus
   every `is_classhow_method`/`.^find_method`/`.^can` allow-list that already
   named `method_table`/`submethod_table`.

Both fixes were verified against `raku` directly (a custom `compose` hook
mixed onto `$class.HOW` via an attribute trait, matching `AttrX::Lazy`'s own
mechanism) and pinned by `t/oo/method/method-table-compose-timing.t` and
`t/oo/method/private-method-table.t`.

With both landed, `AttrX::Lazy`'s own `compose` method now runs its
`method_table`/`private_method_table` checks correctly — but the real
module's end-to-end behavior is still blocked by a separate, newly-found
gap: mutsu applies an attribute's traits (and any `compose` hook they
trigger) inline, in class-body source order, while Rakudo has every method
declaration in the class body already visible by the time the first
attribute trait's `compose` hook runs, regardless of textual order. Filed
as [#8845](https://github.com/tokuhirom/mutsu/issues/8845), out of scope
for this fix.
