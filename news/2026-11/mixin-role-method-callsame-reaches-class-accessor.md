# `callsame` in a mixed-in role's method reaches the class's attribute accessor

`$obj but role { method output { callsame } }` over a class with `has $.output` answered Nil:
the accessor has no `MethodDef`, so the mixin's dispatch frame had no next candidate. The frame
now ends in the class's auto-generated accessor, which `callsame`/`nextsame` read from the
instance under the mixin. Jupyter::Kernel's `Magic::Filters` (`t/06-magic.rakutest`) depends on it.
