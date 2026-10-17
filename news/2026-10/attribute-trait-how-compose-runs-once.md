# A `compose` hook mixed into the HOW by an attribute trait runs once

An attribute trait that does `$attr.package.HOW does SomeRole` (the AttrX::Lazy
shape) had its `compose` hook run twice: once by the deferred attribute-compose
drain and again by the class-declaration drain. The second run saw the accessor
the first had added and threw "A method 'x' already exists". The class
declaration now skips the second run when the attribute path already did it.

Found with Math::Matrix 0.4.2, whose 11 test files now match rakudo.
