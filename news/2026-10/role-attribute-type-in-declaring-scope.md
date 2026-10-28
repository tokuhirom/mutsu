# A role attribute's type is named by the scope that declares it

`unit role Selkie::App::Internal::ErrorLog` imports `NativeCall` and declares
`has Pointer $!saved-windows-stderr-handle`. A class in a file that does not import `Pointer`
composed it and died at `.new` with "Type check failed in assignment to $!...; expected
Pointer but got NativeCall::Types::Pointer": the role's attribute type was recorded as the
raw spelling `Pointer` and resolved later, in the composing class's scope. A class
attribute is already stored under the name its declaring scope maps the spelling to
(`lexical_type_storage_name`); a role attribute now is too (#15207, Selkie `t/107-error-log`).
The remaining blocker of that test, a `try`-wrapped call to an imported routine in a
`unit role` whose `use` follows the unit declaration, is #15291.
