# Enum members reach the enumeration roles' rows by payload-kind shape

An enum member (`ValueView::Enum`) now has an opaque dispatch shape by its
payload kind (`EnumNumeric`, `EnumStringy`, `EnumGeneric`) whose owners are the
roles Rakudo composes into every enum type (`NumericEnumeration` or
`StringyEnumeration`, then `Enumeration`), so the generic opaque site answers
those rows from every entry and `enumeration_owners_of` is gone. A role the
enum composes still wins over an enumeration row (`user_shadows_builtin`).
The base type's row on the member's value stays in `dispatch_enum_method`: a
member's `WHICH` and the `Cool` text methods read through its key are answered
before it, which a receiver-step redirect to the payload would take over.
