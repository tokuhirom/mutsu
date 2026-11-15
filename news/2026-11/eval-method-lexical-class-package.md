# Method EVAL declares lexical classes in the method's package

Methods now enter their declaring package consistently, including when the
class has no package-scoped subs or statics. An EVAL in such a method therefore
names `my class X` as `Owner::X`, matching the package used to compile the EVAL.
Inherited methods keep their original owner's package, while composed role and
mixin methods retain their role scopes. The old per-class probes and metadata
used only to decide whether to enter the declaring package have been removed
(#17609).
