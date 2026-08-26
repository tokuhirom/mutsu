# `EnumHOW` introspection, and the `.^elems` stack overflow

`Type/Metamodel/EnumHOW.rakudoc` documents four introspection methods. mutsu had
only `enum_value_list`; the other three were absent from the `is_classhow_method`
allow-list, so they never reached `dispatch_classhow_method` at all:

```raku
enum Numbers <10 20>;
say Numbers.^enum_values;        # {10 => 0, 20 => 1}
say Numbers.^elems;              # 2
say Numbers.^enum_from_value(0); # 10
```

`.^enum_values` and `.^enum_from_value` died with "No such method". `.^elems`
was worse: it **aborted the process** with a Rust stack overflow, every run.

## The crash

A `rust-gdb -batch` backtrace showed an exact four-frame cycle repeating to
exhaustion:

```
dispatch_elems_method  (methods_dispatch_match2.rs)
  -> call_function("elems")
  -> builtin_elems      (builtins_collection.rs)
  -> call_method_with_values(target, "elems")
  -> dispatch_method_by_name_2 -> dispatch_elems_method -> ...
```

`builtin_elems` is deliberately defined as "`elems($x)` is `$x.elems`" — it
delegates to the method so there is one implementation. `dispatch_elems_method`
delegated straight back to `elems($x)`. That pairing is unproductive by
construction, and for most receivers it never fires only because the native arity
cascade answers first. A metamodel HOW *instance* bypasses that cascade
(`native_fastpath_receiver_state_guard` routes `elems` on any `Instance` to the
interpreter), so `C.^elems` — a class, not even an enum — fell into the cycle and
never came out.

Both halves are fixed. `elems` now appears in `is_classhow_method`, so a HOW
receiver is answered by `dispatch_classhow_method` before it can reach the
generic handler: the enum's value count for an `EnumHOW`, and the inherited
`Any.elems` (`1`) for any other metaobject, which is what `C.HOW.elems` is in
raku. And `dispatch_elems_method` now declines an argument-carrying call outright
instead of re-entering — `.elems` takes no arguments, so a MOP-shaped
`$obj.HOW.elems($obj)` that no HOW serves must become a real missing-method
error, not a recursion.

## The three methods

Implemented next to the existing `enum_value_list` arm, all reading the same
declaration-ordered `(key, value)` variant list out of `registry.enum_types`
(`src/runtime/methods_enumhow.rs` holds the shared lookup):

* `.^enum_values` — a Map from each value's *name* to its underlying value.
  `Numbers.^enum_values` is `{"10" => 0, "20" => 1}`; for a string-valued enum,
  `(p => 'x', q => 'y').^enum_values` is `{:p("x"), :q("y")}`.
* `.^elems` — the number of declared values.
* `.^enum_from_value($v)` — the enum *value object* (not its name) whose
  underlying value is `$v`, or `Mu` when none matches.

Raku reports `C.^enum_values` on a non-enum as a missing method; mutsu reports
the equivalent unresolvable-caller error naming the method and its owner.

Pinned by `t/enum-role-and-enumhow.t`, which passes verbatim under both `raku`
and mutsu, and which reaches its final assertion at all only because the
overflow is gone.
