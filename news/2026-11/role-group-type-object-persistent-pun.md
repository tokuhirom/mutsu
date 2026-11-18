# A role group's type object puns its plain candidate once, so threads no longer race on it

A method called on the type object of a role group that has a plain candidate
next to parametric ones (`role U[\d, \u] {...}; role U { method value(...) {...} }; U.value(...)`,
the shape of `CSS::Units`) used to compose a class named after the role, run
the method, and withdraw the class again, on every call. With the type
registry shared between threads, one worker's withdrawal removed the class
another worker was dispatching through, and the call died with
"No such method 'value'". CSS::Properties' `t/threads.t` failed two of its four
tests that way (#18160).

Such a group now puns its zero-parameter candidate to the same persistent
`R\0pun` class a lone plain role already uses (#18038), so the call neither
writes the registry nor races. CSS::Properties `t/threads.t` matches raku again.
