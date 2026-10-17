# A method's typed `is copy` parameter type-checks stores into it

```raku
class C { method m(Int $x is copy) { $x = 's' } }
C.m(1);   # dies: Type check failed in assignment to $x
```

A store into a sub's typed `is copy` parameter was checked, but a method's
was not. Most method calls bind their parameters through a fast binder that
installed the values without the record of the parameter's type that the
store reads. The fast binder now records it for a typed scalar `is copy`
parameter, positional or named (#13783).
