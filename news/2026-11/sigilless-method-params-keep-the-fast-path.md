# Sigilless method parameters keep the method fast path for values

A method with a sigilless parameter (`method is-set(Mu \obj)`) always took the
full binder, because the compiled-method fast path treats every raw parameter
as one that may alias a caller variable. When the argument is a value -- a
literal, `self`, a type object, a call result -- there is nothing to alias:
the parameter is a read-only term.

Such a call now keeps the fast path, binding the parameter as a read-only term
with the same read-only mark the general binder records. The decision is made
per call: an argument the call site wrote as a variable (the argument-source
names say which) still takes the full binder, which aliases it.

`self.m(self)` with `method m(Mu \i)` went from 36,400 to 19,900 instructions
per call (release build), the same as the `$i` spelling of that method.
AttrX::Mooish's construct/predicate/clear round, which calls methods of this
shape with `self`, moved by about 1%: most of its time is elsewhere.
