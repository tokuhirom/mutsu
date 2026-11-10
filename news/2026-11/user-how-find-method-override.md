# A user HOW's `find_method` override now answers method calls on its types

A metaclass declared through `HOW.new_type` or an `EXPORTHOW` mapping whose class
overrides `find_method` had its override ignored for ordinary method calls: the
method was resolved natively, so wrappers installed by `find_method` never ran
(OO::Actors' `actor` ran every method unserialized, making `t/basic.rakutest`
nondeterministic). The class now records such a HOW and `user_find_method_lookup`
asks `HOW.find_method(type, name)` for each call, invoking a found Callable with
the receiver and the call's arguments. A `Nil` answer falls through to ordinary
dispatch.
