# Qualified calls honor routine `CALL-ME` roles

Package-qualified calls now dispatch through a `CALL-ME` method mixed into an `our sub` by a
trait. This also lets a qualified call reach the role method when the routine body is a stub.
