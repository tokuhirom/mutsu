# `Date` subclass constructors and arithmetic now match Rakudo

`todo/tickets/dist-test-suite-failures-batch.md` listed `Date::YearDay` in the
un-triaged `test_fail` bucket. Its module is a `class Date::YearDay is Date`
whose own `multi method new` builds the object as
`self.Date::new($year - 1, 12, 31) + $day_of_year` — a qualified constructor
call to the builtin ancestor's `new`, followed by Date arithmetic on the
result. Two separate, general bugs broke this shape:

1. **`self.Date::new(...)` (or any `self.Builtin::new(...)`) fell back to
   unqualified dispatch.** From a type object mid-construction, that re-entered
   the caller's OWN overriding `new` — for `Date::YearDay` this went
   `candidate 1 called` → falls through to the default constructor → `Default
   constructor for 'Date::YearDay' only takes named arguments`. From an
   existing instance it just failed outright: `No such method 'Date::new'`.
   `try_qualified_native_ancestor_method`'s whitelist only covered a handful
   of native ancestor types (`IO::Handle`, `Thread`, `VM`, ...), never `Date`.
   Fixed generally in `src/runtime/methods_qualified.rs`: both the type-object
   path (`dispatch_qualified_non_instance_method`) and the instance path
   (`dispatch_qualified_instance_method`) now special-case `actual_method ==
   "new"` for a qualifier with no user-defined constructor — build the
   ancestor via `dispatch_new`, then bless the result as the RECEIVER's own
   type, matching Rakudo's `Date.new`, whose implementation is
   `self.bless(...)` with `self` dynamically the subclass.
2. **Date `+`/`-` arithmetic recognized only the literal class name `"Date"`
   as an operand.** `is_temporal_operand`/`instance_days` in
   `src/builtins/arith/temporal.rs` gated on `class_name == "Date"`, so even a
   correctly-built `Date` subclass instance was never recognized as
   date-like at all — `$date_subclass_instance + 5` silently fell through to
   plain numeric coercion and returned an `Int`. Fixed by duck-typing on the
   `days`/`year` attributes Date always carries (mirroring how
   `instance_datetime_parts` already duck-types DateTime) instead of the
   literal class name. The arithmetic result also now clones the *original*
   operand's full attribute set and keeps its class name — mirroring
   Rakudo's `Date::infix:<+>`, which is `self.clone(:days(...))` — instead of
   always rebuilding a plain `Date` that discarded the subclass type and any
   custom attributes.

Pinned by `t/date-subclass-qualified-new-and-arith.t`. `Date::YearDay`'s own
test suite (`t/01-test-new.t`, 8 subtests) now passes cleanly under mutsu,
matching raku.
