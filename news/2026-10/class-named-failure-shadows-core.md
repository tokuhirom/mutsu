# A top-level `class Failure` shadows the built-in

A `class Failure { ... }` declared at the top level of a unit is now compiled as
a lexical declaration, so `Failure.new(:file(...))` builds the user class
instead of the natively represented core `Failure` (#13141). `fail` keeps
returning the core Failure.
