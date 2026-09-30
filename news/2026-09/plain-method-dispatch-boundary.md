# Plain methods keep their own dispatcher boundary

Every method call now has a deferral frame when a program can use `callsame` or another dispatcher builtin, even when the method has no next candidate. A deferral in an inner plain method therefore returns `Nil` without consuming an enclosing multi routine or method's next candidate. The frame remains lazy on the usual call path.
