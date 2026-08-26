# A role's own method was unreachable once the composing class had a body

The reported symptom was that a role's 0-arg `multi method notes()` "loses a trailing
string literal" — the doc example's output was correct up to its very last character —
and that it only happened when the composing class declared an extra attribute of its
own.

## Root cause — not a literal, and not about `multi`

The method was not losing a character; it was **never being called**. `$j.notes` was
answering from the auto-generated accessor for the role's `has Str $.notes`, which
returns the raw attribute. That value happens to be the method's result minus the
trailing `"\n"` the method appends, which is why the failure looked like a truncated
string literal.

`resolve_user_method_or_accessor` (`src/runtime/class_introspection.rs`) ranks candidates
within one MRO level as: explicit class-local method > public attribute accessor >
role-composed method. The "class entities beat role entities" half of that (6.c
`S14-roles/attributes.t`, "Class prioritization") is correct — but it was applied to
attributes contributed by a composed ROLE as well as to class-declared ones. When a role
declares both `has Str $.notes` and `method notes`, the accessor therefore outranked the
role's own method; raku answers from the method.

The "only with an extra attribute" condition was a red herring, and so was `multi`: a
plain `method` and a class body containing only an unrelated `method zz { 1 }` reproduce
it just as well. What the class body actually controls is whether the accessor column is
ever synced (`Registry::sync_accessor_entries` runs from the class-body walk), so a
class with a completely empty body accidentally behaved correctly — the accessor simply
did not exist yet.

## Fix

The accessor now outranks a role-composed method only when the attribute really is a
class entity. A new `attribute_is_role_contributed` asks whether any of the class's
composed roles declares that attribute; when it does, and a role-composed method of the
same name exists, the method wins. A class-declared attribute still outranks a role
method, which `t/role-composition-gaps.t` pins alongside the three composing-class shapes
(empty body, extra attribute, extra method).
