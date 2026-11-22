# `self!m` resolves in the calling class, not the receiver's

Private methods are neither inherited nor overridden. An unqualified `self!m`
in class `P`'s method names `P`'s own `!m`, but mutsu walked the receiver's
MRO, so a subclass that declared a private method of the same name took the
call: `Child.new.reveal(1)` answered `child 1` instead of `parent 1` (#18831).

Resolution now asks the calling class first, which is the running method's
class or the package a closure was written in. A role's method maps to the class
in the receiver's MRO that composes the role, and that class's own `!m` still
overrides the role's. The receiver walk is only the fallback when the caller has
no such method, so its zero-argument memo stays caller-independent. The typed
resolution cache keys on the caller when it is not the receiver's class.
