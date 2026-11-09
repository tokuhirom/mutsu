# `^add_role` installs the role's attribute accessors

A class made through the MOP (`Metamodel::ClassHOW.new_type`, `^add_role(R)`,
`^compose`) took the role's attributes but not their accessors in the method
table, so a public attribute named like a built-in method (`has $.sort`, over
`Any.sort`) answered the built-in: `$obj.sort` gave a `Seq` of the object, and
Qwiratry's `Rank` / `DenseRank` died on `$.sort.count`. `add_role_to_class_with_args`
now syncs the class's accessor entries after composing the role, as a declared
class does. Qwiratry `t/unit/aggregate-rank.rakutest` and `aggregate-dense-rank`
no longer die there (#13370).
