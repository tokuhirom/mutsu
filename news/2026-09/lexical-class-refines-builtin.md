# A lexical class can refine a CORE type of the same name

`my class DateTime is DateTime { }` declares a lexical subclass that shadows
the CORE `DateTime`: the `is` parent is the outer CORE type, not the class
being declared. mutsu already allowed this when the shadowed type was a
registered class, role or enum. It rejected it with `'DateTime' cannot inherit
from itself.` when the type was one mutsu models natively (`DateTime`, `Str`,
`Int`, ...).

DateTime::strftime builds its `:refine` export this way, so the module could
not even load. With the fix, all three of its test files pass (78/78
assertions).
